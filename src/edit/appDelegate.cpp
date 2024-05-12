// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

#include "appConfig.h"
#include "appDelegate.h"
#include "appState.h"

#include "common.h"
#include "editor.h"
#include "glUtil.h"
#include "overlayLayer.h"
#include "overlayProgram.h"
#include "scene.h"

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
    // Construct scene.
    //

    // Stage needs to be available for app state initialization.
    Scene scene{ appEnvironment->getResolvedInputPath( ), appEnvironment->camera };

    //
    // Init app state.
    //

    AppState appState = { appConfig.screenW, appConfig.screenH, false, false, 0.0, 0.0 };
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

    Editor editor{ };

    //
    // Event loop.
    //

    ImGuiIO & io = ImGui::GetIO( );

    double frame = appEnvironment->frameFirst;
    double frameStep = 1.0 / scene.stage->GetFramesPerSecond( );
    while ( appState.quit == false )
    {
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

        //
        // Draw and blit scene.
        //

        // DrawTarget saves/sets up/restores OpenGL state explicitly
        // via Bind/Unbind.  But it still seems to modify VAO state,
        // so we maintain a separate VAO for it.
        GL_CALL( glBindVertexArray( vaoDrawTarget ) );

        scene.draw( frame, &appState );

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

        editor.draw( );

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

        frame += frameStep;
        if ( frame > appEnvironment->frameLast ) frame = appEnvironment->frameFirst;
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
}

}   // namespace mopr
