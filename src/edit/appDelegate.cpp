// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

#include "clientInProcessECL.h"
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
#include "imgui_impl_sdl2.h"

#include "SDL.h"
#include "SDL_image.h"
#include "SDL_opengl.h"

#include <stddef.h>
#include <vector>

namespace mopr
{

void
 appDelegate( SDL_Window * window, const AppEnvironment * appEnvironment )
{
    GLint fboWindow;
    GL_CALL( glGetIntegerv( GL_FRAMEBUFFER_BINDING, &fboWindow ) );

    auto const & appConfig = mopr::AppConfig::GetInstance( );

    //
    // Initialize MOPR backend.
    //

    const std::string & resolvedWorkshopPath = appEnvironment->getResolvedWorkshopPath( );

    const Client * cli = nullptr;
    if ( appEnvironment->getPortNumber( ) )
    {
        cli = static_cast< Client * >( new ClientLoopbackHTTP(
         resolvedWorkshopPath.c_str( ), appEnvironment->getPortNumber( ) ) );
    }
    else
    {
        cli = static_cast< Client * >(
         new ClientInProcessECL( resolvedWorkshopPath.c_str( ) ) );
    }

    if ( cli->validate( ) )
    {
        printf( "Couldn't validate client!\n" );
        exit( -1 );
    }

    //
    // Init app state.
    //

    AppState appState{ appEnvironment, &appConfig };

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
    // Construct scene.
    //

    pxr::SdfLayerRefPtr layer;

    // Stage needs to be available for app state initialization.
    if ( const ClientInProcess * cliInProcess =
          dynamic_cast< const ClientInProcess * >( cli ) )
    {
        printf( "Populating preview layer using direct method.\n" );

        layer = pxr::SdfLayer::CreateAnonymous( );

        MoprLayer sLayer;
        sLayer.SetRefPtr( layer );
        cliInProcess->execProcedure(
         assetMessaging.uuid.c_str( ), ( void * ) &sLayer, 1 );
    }
    else if ( appConfig.allowExportBasedPreview
              && dynamic_cast< const ClientLoopbackHTTP * >( cli ) )
    {
        printf( "Populating preview layer using export method.\n" );

        std::string wsRelUsdFilePath;
        assetMessaging.exportToUsd( wsRelUsdFilePath );
        std::string absUsdFilePath = resolvedWorkshopPath;
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

    Scene scene{ layer, appState.camera };

    // Update viewport transform based on stage contents.
    if ( appConfig.enableFrameAll )
    {
        scene.frameAll( appState.viewTranslate );
    }

    //
    // Representation classes.
    //

    assetMessaging.initInteraction( );

    // Populated and cleaned up on the Lisp side.
    CommandQueue commandQueue;

    std::vector< std::string > commandOptions;

    commandQueue.clear( );
    assetMessaging.populateEditorLayout( commandQueue, 640, 960 );
    // commandQueue.debugPrint();

    //
    // Init scene.
    //

    GLuint vaoDrawTarget;
    GL_CALL( glGenVertexArrays( 1, &vaoDrawTarget ) );
    GL_CALL( glBindVertexArray( vaoDrawTarget ) );
    if ( !scene.init( &appState ) )
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

    static const double refreshRate = 60.0;
    static const double timeStepMS = 1000.0 / refreshRate;
    const double frameStepMS = 1000.0 / scene.stage->GetFramesPerSecond( );

    // Ensure initial update by subtracting refresh wait time.
    Uint32 lastRenderedTick = SDL_GetTicks( ) - timeStepMS;

    // Rely on last-frame-wraparound to ensure rendering the very first frame first.
    double frameToRender = appState.frameLast + 1;

    unsigned int idPrev = appState.idSelected;
    unsigned int idSubPrev = appState.idSubSelected;
    unsigned int optSelected = 0;
    while ( appState.quit == false )
    {
        // TODO: Improve update scheduling.
        Uint32 currentTick = SDL_GetTicks( );
        Uint32 delta = currentTick - lastRenderedTick;
        if ( delta <= timeStepMS )
        {
            SDL_Delay( 1 );
            continue;
        }
        else
        {
            frameToRender += delta / frameStepMS;
            lastRenderedTick = currentTick;
        }

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
            ImGui_ImplSDL2_ProcessEvent( &e );

            switch ( e.type )
            {
                case SDL_QUIT:
                    appState.quit = true;
                    break;
                case SDL_MOUSEBUTTONDOWN:
                    if ( !io.WantCaptureMouse ) handleMouseButton( &appState, &e );
                    break;
                case SDL_MOUSEBUTTONUP:
                    if ( !io.WantCaptureMouse ) handleMouseButton( &appState, &e );
                    break;
                case SDL_MOUSEMOTION:
                    if ( !io.WantCaptureMouse ) handleMouseMotion( &appState, &e );
                    break;
                case SDL_KEYUP:
                    if ( !io.WantCaptureKeyboard ) handleKeyUp( &appState, &e );
                    break;
                default:
                    break;
            }
        }

        if ( optSelected )
        {
            assetMessaging.applyCommandOption(
             appState.idSelected, appState.idSubSelected, optSelected );

            // Reset.
            optSelected = 0;
        }

        if ( idPrev != appState.idSelected || idSubPrev != appState.idSubSelected )
        {
            commandOptions.clear( );
            if ( appState.idSelected )
            {
                assetMessaging.populateCommandOptions(
                 commandOptions, appState.idSelected, appState.idSubSelected );
            }

            // Reset.
            idPrev = appState.idSelected;
            idSubPrev = appState.idSubSelected;
        }

        //
        // Draw and blit scene.
        //

        // DrawTarget saves/sets up/restores OpenGL state explicitly
        // via Bind/Unbind.  But it still seems to modify VAO state,
        // so we maintain a separate VAO for it.
        GL_CALL( glBindVertexArray( vaoDrawTarget ) );

        scene.draw( frameToRender, &appState );

        GL_CALL( glBindFramebuffer( GL_DRAW_FRAMEBUFFER, fboWindow ) );
        GL_CALL( glBindFramebuffer( GL_READ_FRAMEBUFFER,
                                    scene.drawTarget->GetFramebufferId( ) ) );

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

        // Start ImGui frame.
        ImGui_ImplOpenGL3_NewFrame( );
        ImGui_ImplSDL2_NewFrame( );
        ImGui::NewFrame( );

        editor.drawMenu( );
        editor.drawTree( commandQueue,
                         commandOptions,
                         &appState.idSelected,
                         &appState.idSubSelected,
                         &optSelected );

        // Draw main after the windows that shouldn't be impacted by docking.
        editor.drawMain( );

        ImGui::Render( );
        GL_CALL( glViewport( 0, 0, ( int ) io.DisplaySize.x, ( int ) io.DisplaySize.y ) );

        // NOTE: Call below creates a temporary VAO, and restores last VAO when it is done.
        // Also, it saves/sets up/restores every OpenGL state explicitly.
        ImGui_ImplOpenGL3_RenderDrawData( ImGui::GetDrawData( ) );

        //
        // Finalize.
        //

        GL_CALL( glFinish( ) );

        // Update screen.
        SDL_GL_SwapWindow( window );
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

    assetMessaging.termInteraction( );
    projectMessaging.releaseProject( );
    delete cli;
}

}   // namespace mopr
