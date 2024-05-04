// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

#include "appConfig.h"
#include "appDelegate.h"
#include "appState.h"

#include "common.h"
#include "editor.h"
#include "menu.h"
#include "scene.h"

#include "imgui.h"
#include "imgui_impl_opengl3.h"
#include "imgui_impl_sdl2.h"

#include "SDL.h"
#include "SDL_image.h"
#include "SDL_opengl.h"

namespace mopr
{

void
 appDelegate( SDL_Window * window, const AppEnvironment * appEnvironment )
{
    GLint fboWindow;
    glGetIntegerv( GL_FRAMEBUFFER_BINDING, &fboWindow );
    GLuint vaoDrawTarget;

    Scene scene{ appEnvironment->getResolvedInputPath( ), appEnvironment->camera };

    Menu menu{ };
    Editor editor = { /* .pid = */ 0,
                      /* .vao = */ 0,
                      /* .pos2d = */ -1,
                      /* .vbo = */ 0,
                      /* .ibo = */ 0 };

    auto const & appConfig = mopr::AppConfig::GetInstance( );

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

    {
        glGenVertexArrays( 1, &vaoDrawTarget );

        if ( !scene.init( &appState ) )
        {
            SDL_Log( "Unable to initialize OpenGL state for Usd.\n" );
            return;
        }
    }

    if ( !editor.init( ) )
    {
        SDL_Log( "Unable to initialize OpenGL state for editor.\n" );
        return;
    }

    ImGuiIO & io = ImGui::GetIO( );

    double frame = appEnvironment->frameFirst;
    double frameStep = 1.0 / scene.stage->GetFramesPerSecond( );
    while ( appState.quit == false )
    {
        // Process all queued events.
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

        glBindVertexArray( vaoDrawTarget );

        scene.draw( frame, &appState );

        // Blit the resulting color buffer to the window.
        glBindFramebuffer( GL_DRAW_FRAMEBUFFER, fboWindow );
        glBindFramebuffer( GL_READ_FRAMEBUFFER, scene.drawTarget->GetFramebufferId( ) );

        glBlitFramebuffer( 0,
                           0,
                           appState.screenW,
                           appState.screenH,
                           0,
                           0,
                           appState.screenW,
                           appState.screenH,
                           GL_COLOR_BUFFER_BIT,
                           GL_NEAREST );

        glBindFramebuffer( GL_DRAW_FRAMEBUFFER, fboWindow );
        glBindFramebuffer( GL_READ_FRAMEBUFFER, fboWindow );

        glBindVertexArray( editor.vao );

        // Render showEditor
        if ( appState.showEditor )
        {
            // Bind program
            glUseProgram( editor.pid );

            // Enable vertex position
            glEnableVertexAttribArray( editor.pos2d );

            // Set index data and render
            glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, editor.ibo );
            glDrawElements( GL_TRIANGLES, editor.ibs, GL_UNSIGNED_INT, NULL );

            // Disable vertex position
            glDisableVertexAttribArray( editor.pos2d );

            // Unbind program
            glUseProgram( 0 );
        }

        // Start ImGui frame.
        ImGui_ImplOpenGL3_NewFrame( );
        ImGui_ImplSDL2_NewFrame( );
        ImGui::NewFrame( );

        menu.draw( );

        ImGui::Render( );
        glViewport( 0, 0, ( int ) io.DisplaySize.x, ( int ) io.DisplaySize.y );
        // glClearColor( menu.clearColor.x * menu.clearColor.w,
        //               menu.clearColor.y * menu.clearColor.w,
        //               menu.clearColor.z * menu.clearColor.w,
        //               menu.clearColor.w );
        // glClear( GL_COLOR_BUFFER_BIT );
        ImGui_ImplOpenGL3_RenderDrawData( ImGui::GetDrawData( ) );

        glFinish( );

        // Update screen.
        SDL_GL_SwapWindow( window );

        frame += frameStep;
        if ( frame > appEnvironment->frameLast ) frame = appEnvironment->frameFirst;
    }
}

}   // namespace mopr
