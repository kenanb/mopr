// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

#include "appConfig.h"
#include "appDelegate.h"
#include "appEnvironment.h"
#include "guiConfig.h"

#include "client.h"
#include "clientInProcessECL.h"
#include "clientLoopbackHTTP.h"

#include "imgui.h"
#include "imgui_impl_opengl3.h"
#include "imgui_impl_sdl3.h"

#include "SDL3/SDL.h"
#include "SDL3/SDL_opengl.h"

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

    //
    // Initialize backend.
    //

    const std::string & resolvedWorkshopPath = appEnvironment.getResolvedWorkshopPath( );

    const mopr::Client * cli = nullptr;
    if ( appEnvironment.getPortNumber( ) )
    {
        cli = static_cast< mopr::Client * >( new mopr::ClientLoopbackHTTP(
         resolvedWorkshopPath.c_str( ), appEnvironment.getPortNumber( ) ) );
    }
    else
    {
        cli = static_cast< mopr::Client * >(
         new mopr::ClientInProcessECL( resolvedWorkshopPath.c_str( ) ) );
    }

    if ( cli->validate( ) )
    {
        printf( "Couldn't validate client!\n" );
        return EXIT_FAILURE;
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

    // Initialize SDL
    //

    // NOTE: I observe an issue with SDL2 when initializing SDL_INIT_AUDIO.
    // Startup seems to hang as of 2.30, waiting for the semaphore introduced
    // to PULSEAUDIO_DetectDevices in 82ce05ad. sdl2-2.28.5 works.
    if ( !SDL_Init( SDL_INIT_VIDEO ) )
    {
        SDL_Log( "SDL could not initialize! SDL_Error: %s\n", SDL_GetError( ) );
    }
    else
    {
        setGraphicsAttributes( );

        // From 2.0.18: Enable native IME.
#ifdef SDL_HINT_IME_SHOW_UI
        SDL_SetHint( SDL_HINT_IME_SHOW_UI, "1" );
#endif

        SDL_Log( "SDL Base Path: %s\n", SDL_GetBasePath( ) );

        Uint32 windowFlags = SDL_WINDOW_OPENGL;
        // | SDL_WINDOW_RESIZABLE
        // | SDL_WINDOW_ALLOW_HIGHDPI

        SDL_Window * window = SDL_CreateWindow( "MOPR Editor",
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
                    if ( !SDL_GL_SetSwapInterval( 1 ) )
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
                        // ( void ) io;

                        // TODO : Revisit.
                        io.IniFilename = nullptr;

                        // Enable Keyboard Controls.
                        io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;
                        io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;

                        uiSetStyle( );

                        ImGui_ImplSDL3_InitForOpenGL( window, ctx );

                        const char * glsl_version = "#version 150 core";
                        ImGui_ImplOpenGL3_Init( glsl_version );
                    }

                    auto & guiConfig = mopr::GuiConfig::Instance( );

                    if ( guiConfig.Init( &appEnvironment, &appConfig ) )
                    {
                        printf( "Successfully initialized GUI configuration.\n" );
                    }
                    else
                    {
                        printf(
                         "Some GUI configuration error[s] resulted in fallback "
                         "settings beig used.\n" );
                    }

                    // Run application delegate.
                    //

                    mopr::appDelegate( window, &appEnvironment, cli );

                    // Teardown ImGui.
                    //

                    {
                        ImGui_ImplOpenGL3_Shutdown( );
                        ImGui_ImplSDL3_Shutdown( );
                        ImGui::DestroyContext( );
                    }
                }

                SDL_GL_DestroyContext( ctx );
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

    SDL_Quit( );
    delete cli;
    return EXIT_SUCCESS;
}
