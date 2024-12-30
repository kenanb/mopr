// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

#include "client_ecl.h"

#include "appConfig.h"
#include "appDelegate.h"
#include "appState.h"

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
    Client_ECL_initBackend( resolvedWorkshopPath.c_str( ) );

    //
    // Construct scene.
    //

    // TODO : Replace with real workshop-based path resolution logic.
    const std::string & usdsPath = appEnvironment->getResolvedWorkshopPath( ) + "/"
                                   + appEnvironment->getProjectPath( ) + "/"
                                   + appEnvironment->getResourcePath( );

    pxr::SdfLayerRefPtr layer = pxr::SdfLayer::CreateAnonymous( );
    MoprLayer sLayer;
    sLayer.SetRefPtr( layer );
    Client_ECL_populateFromLispFile( ( void * ) &sLayer, usdsPath.c_str( ), 1 );
    if ( !layer )
    {
        printf( "Couldn't populate layer!\n" );
        exit( -1 );
    }

    // Stage needs to be available for app state initialization.
    Scene scene{ layer, appEnvironment->camera };

    //
    // Representation classes.
    //

    Client_ECL_initRepr( );

    // Populated and cleaned up on the Lisp side.
    CommandQueue commandQueue;
    commandQueue.nofCommands = 0;
    commandQueue.commands = NULL;
    commandQueue.pixelsW = 640.0;
    commandQueue.pixelsH = 960.0;

    CommandOptions commandOptions;
    commandOptions.nofOptions = 0;
    commandOptions.options = NULL;

    Client_ECL_populateCommandQueue( &commandQueue );
    // mopr_print_command_queue( &commandQueue );

    //
    // Init app state.
    //

    AppState appState{ appConfig.screenW, appConfig.screenH };

    appState.viewRotate[ 0 ] = appConfig.viewRotate[ 0 ];
    appState.viewRotate[ 1 ] = appConfig.viewRotate[ 1 ];
    if ( appConfig.enableFrameAll )
    {
        scene.frameAll( appState.viewTranslate );
    }
    else
    {
        appState.viewTranslate[ 0 ] = appConfig.viewTranslate[ 0 ];
        appState.viewTranslate[ 1 ] = appConfig.viewTranslate[ 1 ];
        appState.viewTranslate[ 2 ] = appConfig.viewTranslate[ 2 ];
    }

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

    ImGuiIO & io = ImGui::GetIO( );
    FontInfo fontInfos[ FONT_ROLE_TERMINATOR ];

    {
        int fontSize = appConfig.fontBaseSize;
        fontInfos[ FONT_ROLE_DEFAULT ].fontSize = fontSize;

        if ( appConfig.fontDefault.empty( ) )
        {
            fontInfos[ FONT_ROLE_DEFAULT ].fontPtr = NULL;
        }
        else
        {
            std::string relFontPath = "res/font/";
            relFontPath += appConfig.fontDefault;
            relFontPath += ".ttf";
            std::string absFontPath =
             appEnvironment->resolveAppRelativePath( relFontPath.c_str( ) );
            fontInfos[ FONT_ROLE_DEFAULT ].fontPtr =
             io.Fonts->AddFontFromFileTTF( absFontPath.c_str( ), fontSize );
        }
    }

    {
        int fontSize = appConfig.fontBaseSize + 4;
        fontInfos[ FONT_ROLE_HEADING ].fontSize = fontSize;

        if ( appConfig.fontHeading.empty( ) )
        {
            fontInfos[ FONT_ROLE_HEADING ].fontPtr = NULL;
        }
        else
        {
            std::string relFontPath = "res/font/";
            relFontPath += appConfig.fontHeading;
            relFontPath += ".ttf";
            std::string absFontPath =
             appEnvironment->resolveAppRelativePath( relFontPath.c_str( ) );
            fontInfos[ FONT_ROLE_HEADING ].fontPtr =
             io.Fonts->AddFontFromFileTTF( absFontPath.c_str( ), fontSize );
        }
    }

    Editor editor{ fontInfos };

    //
    // Event loop.
    //

    static const double refreshRate = 60.0;
    static const double timeStepMS = 1000.0 / refreshRate;
    const double frameStepMS = 1000.0 / scene.stage->GetFramesPerSecond( );

    // Ensure initial update by subtracting refresh wait time.
    Uint32 lastRenderedTick = SDL_GetTicks( ) - timeStepMS;

    // Rely on last-frame-wraparound to ensure rendering the very first frame first.
    double frameToRender = appEnvironment->frameLast + 1;

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

        if ( frameToRender > appEnvironment->frameLast )
        {
            frameToRender = appEnvironment->frameFirst;
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
            Client_ECL_applyOption(
             appState.idSelected, appState.idSubSelected, optSelected );

            // Reset.
            optSelected = 0;
        }

        if ( idPrev != appState.idSelected || idSubPrev != appState.idSubSelected )
        {
            Client_ECL_destructCommandOptions( &commandOptions );
            if ( appState.idSelected )
            {
                Client_ECL_populateCommandOptions(
                 &commandOptions, appState.idSelected, appState.idSubSelected );
                // mopr_print_command_options( &commandOptions );
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

        editor.draw( &commandQueue, &appState.idSelected, &appState.idSubSelected );
        if ( commandOptions.nofOptions )
        {
            editor.drawOptions( &commandOptions, &optSelected );
        }

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

    Client_ECL_termRepr( );
    Client_ECL_termBackend( );
    Client_ECL_destructCommandQueue( &commandQueue );
    Client_ECL_destructCommandOptions( &commandOptions );
}

}   // namespace mopr
