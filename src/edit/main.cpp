// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

#include "appConfig.h"
#include "appDelegate.h"
#include "appEnvironment.h"

#include "imgui.h"
#include "imgui_impl_opengl3.h"
#include "imgui_impl_sdl2.h"

#include "SDL.h"
#include "SDL_image.h"
#include "SDL_opengl.h"

#include "pxr/pxr.h"

#include "pxr/imaging/glf/contextCaps.h"
#include "pxr/imaging/glf/diagnostic.h"

static char const * const USAGE =
#include "USAGE"
 ;

void
 setGraphicsAttributes( )
{
    SDL_GL_SetAttribute( SDL_GL_CONTEXT_FLAGS, 0 );
    SDL_GL_SetAttribute( SDL_GL_CONTEXT_MAJOR_VERSION, 3 );
    SDL_GL_SetAttribute( SDL_GL_CONTEXT_MINOR_VERSION, 2 );
    // USD still uses Compatibility profile:
    // https://github.com/PixarAnimationStudios/OpenUSD/pull/2550
    SDL_GL_SetAttribute( SDL_GL_CONTEXT_PROFILE_MASK,
                         SDL_GL_CONTEXT_PROFILE_COMPATIBILITY );

    // Create window with graphics context
    SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
    SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 24 );
    SDL_GL_SetAttribute( SDL_GL_STENCIL_SIZE, 8 );
}

bool
 initGraphicsApi( )
{
    GarchGLApiLoad( );
    pxr::GlfRegisterDefaultDebugOutputMessageCallback( );
    pxr::GlfContextCaps::InitInstance( );
    // If code depends on a GL version or extension:
    //
    // #if defined( GL_VERSION_4_0 )
    //     if ( GARCH_GLAPI_HAS( VERSION_4_0 ) )
    //     {
    //         // ...
    //     }
    // #endif
    //
    // #if defined( GL_ARB_tessellation_shader )
    //     if ( GARCH_GLAPI_HAS( ARB_tessellation_shader ) )
    //     {
    //         // ...
    //     }
    // #endif

    return true;
}

void
 uiSetStyle( )
{
    ImGui::StyleColorsLight( );
    ImVec4 * colors = ImGui::GetStyle( ).Colors;

    colors[ ImGuiCol_WindowBg ] = ImVec4( 0.75f, 0.75f, 0.75f, 0.9f );
}

int
 main( int argc, char * argv[] )
{
    printf(
     "Mopr Editor\n"
     "Copyright (c) 2024 Kenan Bolukbasi.\n"
     "-----------------------------------\n" );

    // Initialize app.
    //

    mopr::AppEnvironment appEnvironment{ argc, argv };

    switch ( appEnvironment.action )
    {
        case mopr::AppEnvironment::ActionHelpSuccess:
            printf( USAGE );
            return EXIT_SUCCESS;
        case mopr::AppEnvironment::ActionHelpFailure:
            printf( USAGE );
            return EXIT_FAILURE;
        case mopr::AppEnvironment::ActionRun:
        default:
            break;
    }

    // Initialize configuration.
    //

    printf( "Reading configuration.\n" );

    auto & appConfig = mopr::AppConfig::Instance( );

    if ( appConfig.Init( appEnvironment.getResolvedAppConfigPath( ) ) )
    {
        printf( "Loaded configuration.\n" );
    }
    else
    {
        printf( "Configuration error[s]. Terminating program.\n" );
        return EXIT_FAILURE;
    }

    int imgFlags = IMG_INIT_PNG;

    // Initialize SDL
    //

    // NOTE: I observe an issue with SDL2 when initializing SDL_INIT_AUDIO.
    // Startup seems to hang as of 2.30, waiting for the semaphore introduced
    // to PULSEAUDIO_DetectDevices in 82ce05ad. sdl2-2.28.5 works.
    if ( SDL_Init( SDL_INIT_VIDEO | SDL_INIT_AUDIO ) < 0 )
    {
        SDL_Log( "SDL could not initialize! SDL_Error: %s\n", SDL_GetError( ) );
    }
    else if ( ~IMG_Init( imgFlags ) & imgFlags )
    {
        SDL_Log( "IMG could not initialize! SDL_Error: %s\n", IMG_GetError( ) );
    }
    else
    {
        setGraphicsAttributes( );

        // From 2.0.18: Enable native IME.
#ifdef SDL_HINT_IME_SHOW_UI
        SDL_SetHint( SDL_HINT_IME_SHOW_UI, "1" );
#endif

        SDL_Log( "SDL Base Path: %s\n", SDL_GetBasePath( ) );

        Uint32 windowFlags = SDL_WINDOW_SHOWN | SDL_WINDOW_OPENGL;
        // | SDL_WINDOW_RESIZABLE
        // | SDL_WINDOW_ALLOW_HIGHDPI

        SDL_Window * window = SDL_CreateWindow( "SDL Tutorial",
                                                SDL_WINDOWPOS_UNDEFINED,
                                                SDL_WINDOWPOS_UNDEFINED,
                                                appConfig.screenW,
                                                appConfig.screenH,
                                                windowFlags );
        if ( window )
        {
            SDL_GLContext ctx = SDL_GL_CreateContext( window );

            if ( ctx )
            {
                SDL_GL_MakeCurrent( window, ctx );

                if ( initGraphicsApi( ) )
                {
                    // Use Vsync
                    if ( SDL_GL_SetSwapInterval( 1 ) < 0 )
                    {
                        SDL_Log( "Warning: Unable to set VSync! SDL Error: %s\n",
                                 SDL_GetError( ) );
                    }

                    // Setup ImGui.
                    //

                    {
                        IMGUI_CHECKVERSION( );
                        ImGui::CreateContext( );

                        ImGuiIO & io = ImGui::GetIO( );
                        ( void ) io;

                        // Enable Keyboard Controls.
                        io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;

                        uiSetStyle();

                        ImGui_ImplSDL2_InitForOpenGL( window, ctx );

                        const char * glsl_version = "#version 150 core";
                        ImGui_ImplOpenGL3_Init( glsl_version );
                    }

                    // Run application delegate.
                    //

                    mopr::appDelegate( window, &appEnvironment );

                    // Teardown ImGui.
                    //

                    {
                        ImGui_ImplOpenGL3_Shutdown( );
                        ImGui_ImplSDL2_Shutdown( );
                        ImGui::DestroyContext( );
                    }
                }

                SDL_GL_DeleteContext( ctx );
            }
            else
            {
                SDL_Log( "OpenGL context could not be created! SDL Error: %s\n",
                         SDL_GetError( ) );
            }

            SDL_DestroyWindow( window );
        }
        else
        {
            SDL_Log( "Window could not be created! SDL_Error: %s\n", SDL_GetError( ) );
        }
    }

    IMG_Quit( );
    SDL_Quit( );
    return EXIT_SUCCESS;
}
