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

#include "pugixml.hpp"

#include <sstream>
#include <stddef.h>
#include <vector>

namespace mopr
{

unsigned int
 requestGet( pugi::xml_document & docResponse, const char * uri )
{
    const char * response = NULL;

    // printf( "C                  | RESPONSE ADDRESS : %p\n", ( void * ) &response );

    // printf( "C <REQUEST         | RESPONSE POINTER : %p\n", ( void * ) response );
    Client_ECL_requestGet( &response, uri );
    // printf( "C         REQUEST> | RESPONSE POINTER : %p\n", ( void * ) response );

    // printf( "C         REQUEST> | RESPONSE CONTENT : %s\n", response );

    [[maybe_unused]] pugi::xml_parse_result result = docResponse.load_string( response );

    // std::cout << "Query result: " << result.description( ) << std::endl;
    // std::cout << "Document: \n";
    // docResponse.save( std::cout );

    // printf( "C <RELEASE         | RESPONSE POINTER : %p\n", ( void * ) response );
    Client_ECL_releaseResponse( &response );
    // printf( "C         RELEASE> | RESPONSE POINTER : %p\n", ( void * ) response );

    return 0;
}

unsigned int
 requestPost( pugi::xml_document & docResponse,
              const char * uri,
              const pugi::xml_document & docRequest )
{
    const char * response = NULL;
    std::ostringstream requestStream;
    docRequest.save( requestStream );
    const std::string & requestString = requestStream.str( );

    // printf( "C                  | RESPONSE ADDRESS : %p\n", ( void * ) &response );

    // printf( "C <REQUEST         | RESPONSE POINTER : %p\n", ( void * ) response );
    Client_ECL_requestPost( &response, uri, requestString.c_str( ) );
    // printf( "C         REQUEST> | RESPONSE POINTER : %p\n", ( void * ) response );

    // printf( "C         REQUEST> | RESPONSE CONTENT : %s\n", response );

    [[maybe_unused]] pugi::xml_parse_result result = docResponse.load_string( response );

    // std::cout << "Query result: " << result.description( ) << std::endl;
    // std::cout << "Document: \n";
    // docResponse.save( std::cout );

    // printf( "C <RELEASE         | RESPONSE POINTER : %p\n", ( void * ) response );
    Client_ECL_releaseResponse( &response );
    // printf( "C         RELEASE> | RESPONSE POINTER : %p\n", ( void * ) response );

    return 0;
}

std::string
 requestGetAndSelectUri( const char * get, const char * select )
{
    pugi::xml_document doc;
    requestGet( doc, get );
    pugi::xpath_node xp = doc.select_node( select );

    std::string result;
    if ( xp ) result = xp.node( ).attribute( "uri" ).value( );
    return result;
}

std::string
 locateEndpointWorkshop( )
{
    return requestGetAndSelectUri( "/", "//endpoints/endpoint[@name='workshop']" );
}

std::string
 locateResourceWorkshop( const std::string & uriEndpointWorkshop )
{
    return requestGetAndSelectUri( uriEndpointWorkshop.c_str( ), "//workshop" );
}

std::string
 locateEndpointProject( const std::string & uriResourceWorkshop )
{
    return requestGetAndSelectUri( uriResourceWorkshop.c_str( ),
                                   "//endpoints/endpoint[@name='project']" );
}

std::string
 locateResourceProject( const std::string & uriEndpointProject,
                        const AppEnvironment * appEnvironment )
{
    std::string selection = "//projects/project[@path='";
    selection += appEnvironment->getProjectPath( );
    selection += "']";
    return requestGetAndSelectUri( uriEndpointProject.c_str( ), selection.c_str( ) );
}

std::string
 locateEndpointAsset( const std::string & uriResourceProject )
{
    return requestGetAndSelectUri( uriResourceProject.c_str( ),
                                   "//endpoints/endpoint[@name='asset']" );
}

std::string
 locateResourceAsset( const std::string & uriEndpointAsset,
                      const AppEnvironment * appEnvironment )
{
    std::string selection = "//assets/asset[@path='";
    selection += appEnvironment->getAssetPath( );
    selection += "']";
    return requestGetAndSelectUri( uriEndpointAsset.c_str( ), selection.c_str( ) );
}

std::string
 locateEndpointWorktree( const std::string & uriResourceAsset )
{
    return requestGetAndSelectUri( uriResourceAsset.c_str( ),
                                   "//endpoints/endpoint[@name='worktree']" );
}

unsigned int
 queryWorkshop( const AppEnvironment * appEnvironment )
{
    std::string uriEpW = locateEndpointWorkshop( );
    if ( uriEpW.empty( ) ) return 0;
    std::cout << "Workshop endpoint URI : " << uriEpW << std::endl;

    std::string uriResW = locateResourceWorkshop( uriEpW );
    if ( uriResW.empty( ) ) return 0;
    std::cout << "Workshop resource URI : " << uriResW << std::endl;

    std::string uriEpP = locateEndpointProject( uriResW );
    if ( uriEpP.empty( ) ) return 0;
    std::cout << "Project endpoint URI  : " << uriEpP << std::endl;

    std::string uriResP = locateResourceProject( uriEpP, appEnvironment );
    if ( uriResP.empty( ) ) return 0;
    std::cout << "Project resource URI  : " << uriResP << std::endl;

    std::string uriEpA = locateEndpointAsset( uriResP );
    if ( uriEpA.empty( ) ) return 0;
    std::cout << "Asset endpoint URI    : " << uriEpA << std::endl;

    std::string uriResA = locateResourceAsset( uriEpA, appEnvironment );
    if ( uriResA.empty( ) ) return 0;
    std::cout << "Asset resource URI    : " << uriResA << std::endl;

    std::string uriEpWorktree = locateEndpointWorktree( uriResA );
    if ( uriEpWorktree.empty( ) ) return 0;
    std::cout << "Worktree endpoint URI : " << uriEpWorktree << std::endl;

    // Testing.
    {
        pugi::xml_document docRequest;

        pugi::xml_document docResponse;
        requestPost( docResponse, uriEpWorktree.c_str( ), docRequest );
        docResponse.save( std::cout );
    }

    return 0;
}

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

    queryWorkshop( appEnvironment );

    //
    // Construct scene.
    //

    // TODO : Replace with real workshop-based path resolution logic.
    const std::string & usdsPath = appEnvironment->getResolvedWorkshopPath( ) + "/"
                                   + appEnvironment->getProjectPath( ) + "/"
                                   + appEnvironment->getAssetPath( );

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

    pugi::xml_document docCommandOptions;

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
                std::string uriCommandOptions = "/";
                uriCommandOptions += "command-options";
                uriCommandOptions += "?id=" + std::to_string( appState.idSelected );
                uriCommandOptions +=
                 "&id-sub=" + std::to_string( appState.idSubSelected );

                requestGet( docCommandOptions, uriCommandOptions.c_str( ) );

                std::cout << "CommandOptions: \n";
                docCommandOptions.save( std::cout );

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
