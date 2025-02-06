// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

#include "clientLoopbackHTTP.h"

#include "appConfig.h"
#include "appDelegate.h"
#include "appState.h"
#include "messaging.h"
#include "viewportNavigation.h"

#include "common.h"
#include "editor.h"
#include "glUtil.h"
#include "overlayLayer.h"
#include "overlayProgram.h"
#include "scene.h"

#include "repr/command.h"

#include "wrap/usd/box/layer.h"

#include "imgui.h"
#include "imgui_impl_opengl3.h"
#include "imgui_impl_sdl3.h"

#include "SDL3/SDL.h"
#include "SDL3/SDL_opengl.h"

#include <stddef.h>
#include <vector>

namespace mopr
{

static const double scRefreshRate = 60.0;
static const double scTimeStepMS = 1000.0 / scRefreshRate;

static pxr::SdfLayerRefPtr
 generateLayer( const AssetMessaging & assetMessaging,
                const AppConfig & appConfig,
                const AppEnvironment * appEnvironment )
{
    pxr::SdfLayerRefPtr layer;

    // Stage needs to be available for app state initialization.
    if ( const ClientInProcess * cliInProcess =
          dynamic_cast< const ClientInProcess * >( assetMessaging.client ) )
    {
        printf( "Populating preview layer using direct method.\n" );

        layer = pxr::SdfLayer::CreateAnonymous( );

        MoprLayer sLayer;
        sLayer.SetRefPtr( layer );
        cliInProcess->execProcedure(
         assetMessaging.uuid.c_str( ), ( void * ) &sLayer, 1 );
    }
    else if ( appConfig.allowExportBasedPreview
              && dynamic_cast< const ClientLoopbackHTTP * >( assetMessaging.client ) )
    {
        printf( "Populating preview layer using export method.\n" );

        std::string wsRelUsdFilePath;
        assetMessaging.exportToUsd( wsRelUsdFilePath );
        std::string absUsdFilePath = appEnvironment->getResolvedWorkshopPath( );
        absUsdFilePath += wsRelUsdFilePath;

        layer = pxr::SdfLayer::OpenAsAnonymous( absUsdFilePath );
    }
    else
    {
        printf(
         "Client configuration doesn't support layer population. "
         "Preview is disabled.\n" );

        layer = pxr::SdfLayer::CreateAnonymous( );
    }

    return layer;
}

struct Asset
{
    AssetMessaging * msg;
    std::string camera;
    unsigned int idSelected;
    unsigned int idSubSelected;
    CommandQueue commandQueue;
    std::vector< std::string > commandOptions;
    Scene scene;

    Asset( AssetMessaging * msg,
           const pxr::SdfLayerRefPtr layer,
           const std::string & cameraPath )
        : msg( msg )
        , camera( cameraPath )
        , idSelected( 0 )
        , idSubSelected( 0 )
        , commandQueue( )
        , commandOptions( )
        , scene( layer, cameraPath )
    {
    }

    Asset( AssetMessaging & assetMessaging,
           const AppConfig & appConfig,
           const AppEnvironment * appEnvironment,
           const std::string & cameraPath )
        : Asset( &assetMessaging,
                 generateLayer( assetMessaging, appConfig, appEnvironment ),
                 cameraPath )
    {
    }
};

void
 appDelegate( SDL_Window * window,
              const AppEnvironment * appEnvironment,
              const Client * cli )
{
    auto const & appConfig = mopr::AppConfig::GetInstance( );
    AppState appState{ appEnvironment, &appConfig };

    //
    // Get framebuffer ID of window.
    //

    GLint fboWindow;
    GL_CALL( glGetIntegerv( GL_FRAMEBUFFER_BINDING, &fboWindow ) );

    //
    // Init messaging.
    //

    WorkshopMessaging workshopMessaging( cli );
    workshopMessaging.initGenericEndpoints( );
    workshopMessaging.debugPrint( );

    ProjectMessaging & projectMessaging =
     workshopMessaging.getOrCreateProjectMessaging( appState.projectPath );
    projectMessaging.acquireProject( );
    projectMessaging.initProjectEndpoints( );
    projectMessaging.debugPrint( );

    AssetMessaging & assetMessaging =
     projectMessaging.getOrCreateAssetMessaging( appState.assetPath );
    assetMessaging.debugPrint( );
    assetMessaging.bindStaging( );

    //
    // Construct asset and corresponding scene.
    //

    Asset * asset = new Asset( assetMessaging, appConfig, appEnvironment, "" );

    // Update viewport transform based on stage contents.
    if ( appConfig.enableFrameAll )
    {
        asset->scene.frameAll( appState.viewTranslate );
    }

    //
    // Representation classes.
    //

    asset->msg->initInteraction( );

    asset->commandQueue.clear( );
    asset->msg->populateEditorLayout( asset->commandQueue, 640, 960 );
    // asset->commandQueue.debugPrint();

    //
    // Init scene.
    //

    GLuint vaoDrawTarget;
    GL_CALL( glGenVertexArrays( 1, &vaoDrawTarget ) );
    GL_CALL( glBindVertexArray( vaoDrawTarget ) );
    if ( !asset->scene.init( &appState ) )
    {
        SDL_Log( "Unable to initialize OpenGL state for Usd.\n" );
        return;
    }

    //
    // Init overlay.
    //

    OverlayProgram overlayProgram{ };

    if ( !overlayProgram.init( ) )
    {
        SDL_Log( "Unable to initialize OpenGL state for overlay program.\n" );
        return;
    }

    std::vector< OverlayLayer > layers;
    dummyOverlay( layers );
    for ( auto & layer : layers )
    {
        GL_CALL( glGenVertexArrays( 1, &layer.vao ) );
        GL_CALL( glBindVertexArray( layer.vao ) );
        layer.init( overlayProgram );
    }

    //
    // Init editor.
    //

    Editor editor;

    //
    // Event loop.
    //

    ImGuiIO & io = ImGui::GetIO( );

    // Ensure initial update by subtracting refresh wait time.
    Uint64 lastRenderedTickMS = SDL_GetTicks( ) - scTimeStepMS;

    // Rely on last-frame-wraparound to ensure rendering the very first frame first.
    double frameToRender = appState.frameLast + 1;

    while ( appState.quit == false )
    {
        // TODO: Improve update scheduling.
        Uint64 currentTickMS = SDL_GetTicks( );
        Uint64 deltaMS = currentTickMS - lastRenderedTickMS;
        if ( deltaMS <= scTimeStepMS )
        {
            SDL_Delay( 1 );
            continue;
        }
        else
        {
            lastRenderedTickMS = currentTickMS;
        }

        //
        // Calculate frame to render.
        //

        frameToRender += deltaMS / asset->scene.frameStepMS( );
        if ( frameToRender > appState.frameLast )
        {
            frameToRender = appState.frameFirst;
        }

        //
        // Process queued events.
        //

        SDL_Event e;

        while ( SDL_PollEvent( &e ) )
        {
            ImGui_ImplSDL3_ProcessEvent( &e );

            switch ( e.type )
            {
                case SDL_EVENT_QUIT:
                    appState.quit = true;
                    break;
                case SDL_EVENT_MOUSE_BUTTON_DOWN:
                    if ( !io.WantCaptureMouse ) handleMouseButton( &appState, &e );
                    break;
                case SDL_EVENT_MOUSE_BUTTON_UP:
                    if ( !io.WantCaptureMouse ) handleMouseButton( &appState, &e );
                    break;
                case SDL_EVENT_MOUSE_MOTION:
                    if ( !io.WantCaptureMouse ) handleMouseMotion( &appState, &e );
                    break;
                case SDL_EVENT_KEY_UP:
                    if ( !io.WantCaptureKeyboard ) handleKeyUp( &appState, &e );
                    break;
                default:
                    break;
            }
        }

        //
        // Draw and blit scene.
        //

        // DrawTarget saves/sets up/restores OpenGL state explicitly
        // via Bind/Unbind.  But it still seems to modify VAO state,
        // so we maintain a separate VAO for it.
        GL_CALL( glBindVertexArray( vaoDrawTarget ) );

        asset->scene.draw( frameToRender, &appState );

        GL_CALL( glBindFramebuffer( GL_DRAW_FRAMEBUFFER, fboWindow ) );
        GL_CALL(
         glBindFramebuffer( GL_READ_FRAMEBUFFER, asset->scene.fboDrawTarget( ) ) );

        GL_CALL( glBlitFramebuffer( 0,
                                    0,
                                    appState.screenW,
                                    appState.screenH,
                                    0,
                                    0,
                                    appState.screenW,
                                    appState.screenH,
                                    GL_COLOR_BUFFER_BIT,
                                    GL_NEAREST ) );

        GL_CALL( glBindFramebuffer( GL_DRAW_FRAMEBUFFER, fboWindow ) );
        GL_CALL( glBindFramebuffer( GL_READ_FRAMEBUFFER, fboWindow ) );

        //
        // Draw overlay layers.
        //

        if ( appState.showOverlays )
        {
            GL_CALL( glUseProgram( overlayProgram.pid ) );
            for ( const auto & layer : layers )
            {
                GL_CALL( glBindVertexArray( layer.vao ) );
                layer.draw( overlayProgram );
            }
            GL_CALL( glUseProgram( 0 ) );
        }

        //
        // Draw editor.
        //

        unsigned int optSelected = 0;
        unsigned int idPrev = asset->idSelected;
        unsigned int idSubPrev = asset->idSubSelected;

        // Start ImGui frame.
        ImGui_ImplOpenGL3_NewFrame( );
        ImGui_ImplSDL3_NewFrame( );
        ImGui::NewFrame( );

        editor.drawMenu( );
        editor.drawTree( asset->commandQueue,
                         asset->commandOptions,
                         &asset->idSelected,
                         &asset->idSubSelected,
                         &optSelected );

        // Draw main after the windows that shouldn't be impacted by docking.
        editor.drawMain( );

        ImGui::Render( );
        GL_CALL( glViewport( 0, 0, ( int ) io.DisplaySize.x, ( int ) io.DisplaySize.y ) );

        // NOTE: Call below creates a temporary VAO, and restores last VAO when it is done.
        // Also, it saves/sets up/restores every OpenGL state explicitly.
        ImGui_ImplOpenGL3_RenderDrawData( ImGui::GetDrawData( ) );

        //
        // Finalize draw.
        //

        GL_CALL( glFinish( ) );

        // Update screen.
        SDL_GL_SwapWindow( window );

        //
        // React to queued UI interactions.
        //

        if ( optSelected )
        {
            asset->msg->applyCommandOption(
             asset->idSelected, asset->idSubSelected, optSelected );
        }

        if ( idPrev != asset->idSelected || idSubPrev != asset->idSubSelected )
        {
            asset->commandOptions.clear( );
            if ( asset->idSelected )
            {
                asset->msg->populateCommandOptions(
                 asset->commandOptions, asset->idSelected, asset->idSubSelected );
            }
        }
    }

    //
    // Cleanup.
    //

    for ( auto & layer : layers )
    {
        layer.fini( );
        GL_CALL( glDeleteVertexArrays( 1, &layer.vao ) );
    }

    GL_CALL( glDeleteVertexArrays( 1, &vaoDrawTarget ) );

    overlayProgram.fini( );

    asset->msg->termInteraction( );
    delete asset;

    projectMessaging.releaseProject( );
}

}   // namespace mopr
