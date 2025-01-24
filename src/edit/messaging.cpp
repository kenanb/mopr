#include "messaging.h"

#include "pugixml.hpp"

#include <iostream>
#include <sstream>

#include <stdlib.h>

namespace mopr
{

MessagingBase::MessagingBase( const Client * client ) : client( client ), base( ), uuid( )
{
}

static unsigned int
 requestGet( const Client * client, pugi::xml_document & docResponse, const char * uri )
{
    char * response = NULL;

    // printf( "C                  | RESPONSE ADDRESS : %p\n", ( void * ) &response );

    // printf( "C <REQUEST         | RESPONSE POINTER : %p\n", ( void * ) response );
    client->requestGet( &response, uri );
    // printf( "C         REQUEST> | RESPONSE POINTER : %p\n", ( void * ) response );

    // printf( "C         REQUEST> | RESPONSE CONTENT : %s\n", response );

    [[maybe_unused]] pugi::xml_parse_result result = docResponse.load_string( response );

    // std::cout << "Query result: " << result.description( ) << std::endl;
    // std::cout << "Document: \n";
    // docResponse.save( std::cout );

    // printf( "C <RELEASE         | RESPONSE POINTER : %p\n", ( void * ) response );
    client->releaseResponse( &response );
    // printf( "C         RELEASE> | RESPONSE POINTER : %p\n", ( void * ) response );

    return 0;
}

static unsigned int
 requestPost( const Client * client,
              pugi::xml_document & docResponse,
              const char * uri,
              const pugi::xml_document & docRequest )
{
    char * response = NULL;
    std::ostringstream requestStream;
    docRequest.save( requestStream );
    const std::string & requestString = requestStream.str( );

    // printf( "C                  | RESPONSE ADDRESS : %p\n", ( void * ) &response );

    // printf( "C <REQUEST         | RESPONSE POINTER : %p\n", ( void * ) response );
    client->requestPost( &response, uri, requestString.c_str( ) );
    // printf( "C         REQUEST> | RESPONSE POINTER : %p\n", ( void * ) response );

    // printf( "C         REQUEST> | RESPONSE CONTENT : %s\n", response );

    [[maybe_unused]] pugi::xml_parse_result result = docResponse.load_string( response );

    // std::cout << "Query result: " << result.description( ) << std::endl;
    // std::cout << "Document: \n";
    // docResponse.save( std::cout );

    // printf( "C <RELEASE         | RESPONSE POINTER : %p\n", ( void * ) response );
    client->releaseResponse( &response );
    // printf( "C         RELEASE> | RESPONSE POINTER : %p\n", ( void * ) response );

    return 0;
}

unsigned int
 MessagingBase::requestGetAndSetBaseInfo( const char * get )
{
    pugi::xml_document doc;
    requestGet( client, doc, get );

    const std::string & select = getBaseResourceSelector( );
    pugi::xpath_node xp = doc.select_node( select.c_str( ) );

    if ( xp )
    {
        uuid = xp.node( ).attribute( "uuid" ).value( );
        base = xp.node( ).attribute( "uri" ).value( );
        return 0;
    }
    return 1;
}

std::string
 MessagingBase::requestGetAndSelectUri( const char * get, const char * select ) const
{
    pugi::xml_document doc;
    requestGet( client, doc, get );
    pugi::xpath_node xp = doc.select_node( select );

    std::string result;
    if ( xp ) result = xp.node( ).attribute( "uri" ).value( );
    return result;
}

std::string
 MessagingBase::locateEndpoint( const char * uriResource, const char * endpoint ) const
{
    std::string select = "//endpoints/endpoint[@name='";
    select += endpoint;
    select += "']";
    return requestGetAndSelectUri( uriResource, select.c_str( ) );
}

WorkshopMessaging::WorkshopMessaging( const Client * client )
    : MessagingBase( client ), uriEpW( ), uriEpP( ), projectPathToMessaging( )
{
}

ProjectMessaging &
 WorkshopMessaging::getOrCreateProjectMessaging( const std::string & projectPath )
{
    if ( auto it = projectPathToMessaging.find( projectPath );
         it != projectPathToMessaging.end( ) )
        return it->second;

    ProjectMessaging projectMessaging( client, projectPath );
    projectMessaging.initGenericEndpoints( uriEpP );

    projectPathToMessaging.emplace( projectPath, projectMessaging );
    return projectPathToMessaging.at( projectPath );
}

void
 WorkshopMessaging::debugPrint( ) const
{
    std::cout << "\nWorkshop UUID          : " << uuid
              << "\nWorkshop endpoint URI  : " << uriEpW
              << "\nWorkshop resource URI  : " << base
              << "\nProject endpoint URI   : " << uriEpP << std::endl;
}

unsigned int
 WorkshopMessaging::initGenericEndpoints( )
{
    uriEpW = locateEndpoint( "/", "workshop" );
    if ( uriEpW.empty( ) ) return 1;

    if ( requestGetAndSetBaseInfo( uriEpW.c_str( ) ) ) return 1;

    uriEpP = locateEndpoint( base.c_str( ), "project" );
    if ( uriEpP.empty( ) ) return 1;

    return 0;
}

ProjectMessaging::ProjectMessaging( const Client * client,
                                    const std::string & projectPath )
    : MessagingBase( client )
    , path( projectPath )
    , uriEpPL( )
    , uriEpA( )
    , pLockState( )
    , assetPathToMessaging( )
{
}

AssetMessaging &
 ProjectMessaging::getOrCreateAssetMessaging( const std::string & assetPath )
{
    if ( auto it = assetPathToMessaging.find( assetPath );
         it != assetPathToMessaging.end( ) )
        return it->second;

    AssetMessaging assetMessaging( client, assetPath );
    assetMessaging.initGenericEndpoints( uriEpA );

    assetPathToMessaging.emplace( assetPath, assetMessaging );
    return assetPathToMessaging.at( assetPath );
}

void
 ProjectMessaging::debugPrint( ) const
{
    std::cout << "\nProject UUID           : " << uuid
              << "\nProject PATH           : " << path
              << "\nProject resource URI   : " << base
              << "\nProj lock endpoint URI : " << uriEpPL
              << "\nAsset endpoint URI     : " << uriEpA << std::endl;
}

std::string
 ProjectMessaging::manageProjectLock( const std::string & uriResourceProject,
                                      bool acquire ) const
{
    pugi::xml_document docResponse;
    pugi::xml_document docRequest;

    pugi::xml_node node_action = docRequest.append_child( "action" );
    pugi::xml_attribute attr_action_name = node_action.append_attribute( "name" );
    attr_action_name.set_value( acquire ? "acquire" : "release" );

    requestPost( client, docResponse, uriResourceProject.c_str( ), docRequest );

    pugi::xpath_node xp = docResponse.select_node( "//project-lock" );

    std::string result;
    if ( xp ) result = xp.node( ).attribute( "state" ).value( );
    return result;
}

unsigned int
 ProjectMessaging::acquireProject( )
{
    pLockState = manageProjectLock( uriEpPL, true );
    if ( pLockState != "acquired" ) return 1;

    return 0;
}

unsigned int
 ProjectMessaging::releaseProject( )
{
    pLockState = manageProjectLock( uriEpPL, false );
    if ( pLockState != "released" ) return 1;

    return 0;
}

unsigned int
 ProjectMessaging::initGenericEndpoints( const std::string & uriEndpointProject )
{
    if ( requestGetAndSetBaseInfo( uriEndpointProject.c_str( ) ) ) return 1;

    uriEpPL = locateEndpoint( base.c_str( ), "lock" );
    if ( uriEpPL.empty( ) ) return 1;

    return 0;
}

unsigned int
 ProjectMessaging::initProjectEndpoints( )
{
    uriEpA = locateEndpoint( base.c_str( ), "asset" );
    if ( uriEpA.empty( ) ) return 0;

    return 0;
}

AssetMessaging::AssetMessaging( const Client * client, const std::string & assetPath )
    : MessagingBase( client )
    , path( assetPath )
    , uriEpStaging( )
    , uriEpWorking( )
    , uriResBound( )
{
}

void
 AssetMessaging::debugPrint( ) const
{
    std::cout << "\nAsset UUID             : " << uuid
              << "\nAsset PATH             : " << path
              << "\nAsset resource URI     : " << base
              << "\nStaging endpoint URI   : " << uriEpStaging
              << "\nWorking endpoint URI   : " << uriEpWorking
              << "\nBound resource URI     : " << uriResBound << std::endl;
}

unsigned int
 AssetMessaging::initGenericEndpoints( const std::string & uriEndpointAsset )
{
    if ( requestGetAndSetBaseInfo( uriEndpointAsset.c_str( ) ) ) return 1;

    uriEpStaging = locateEndpoint( base.c_str( ), "staging" );
    if ( uriEpStaging.empty( ) ) return 1;

    uriEpWorking = locateEndpoint( base.c_str( ), "working" );
    if ( uriEpWorking.empty( ) ) return 1;

    return 0;
}

unsigned int
 AssetMessaging::bindStaging( )
{
    pugi::xml_document docResponse;
    pugi::xml_document docRequest;

    pugi::xml_node node_action = docRequest.append_child( "action" );
    pugi::xml_attribute attr_action_name = node_action.append_attribute( "name" );
    attr_action_name.set_value( "bind" );

    requestPost( client, docResponse, uriEpStaging.c_str( ), docRequest );

    pugi::xpath_node xp = docResponse.select_node( "//asset" );

    uriResBound = xp.node( ).attribute( "uri" ).value( );

    std::string result;
    if ( xp ) result = xp.node( ).attribute( "path" ).value( );
    std::cout << "Loaded asset path: " << result << std::endl;

    return 0;
}

unsigned int
 AssetMessaging::initInteraction( ) const
{
    pugi::xml_document docResponse;
    pugi::xml_document docRequest;

    pugi::xml_node node_action = docRequest.append_child( "action" );
    pugi::xml_attribute attr_action_name = node_action.append_attribute( "name" );
    attr_action_name.set_value( "init-interaction" );

    requestPost( client, docResponse, uriResBound.c_str( ), docRequest );

    return 0;
}

unsigned int
 AssetMessaging::termInteraction( ) const
{
    pugi::xml_document docResponse;
    pugi::xml_document docRequest;

    pugi::xml_node node_action = docRequest.append_child( "action" );
    pugi::xml_attribute attr_action_name = node_action.append_attribute( "name" );
    attr_action_name.set_value( "term-interaction" );

    requestPost( client, docResponse, uriResBound.c_str( ), docRequest );

    return 0;
}

static CommandBase *
 generateCommand( const pugi::xml_node & node )
{
    CommandBase * elt = nullptr;

    switch ( atoi( node.attribute( "c-type" ).value( ) ) )
    {
        case COMMAND_TYPE_DRAW_ROOT_CONTAINER:
        {
            elt = new CommandDrawRootContainer( );
            break;
        }

        case COMMAND_TYPE_DRAW_EXPR_CONTAINER:
        {
            elt = new CommandDrawExprContainer( );
            break;
        }

        case COMMAND_TYPE_DRAW_EXPR_LABEL:
        {
            elt = new CommandDrawExprLabel(
             static_cast< CommandTheme >( atoi( node.attribute( "bg" ).value( ) ) ),
             node.attribute( "text" ).value( ) );
            break;
        }

        case COMMAND_TYPE_DRAW_ATTR_LABEL:
        {
            elt = new CommandDrawAttrLabel(
             static_cast< CommandTheme >( atoi( node.attribute( "bg" ).value( ) ) ),
             node.attribute( "text" ).value( ) );
            break;
        }

        case COMMAND_TYPE_DRAW_ATTR_INPUT:
        {
            elt = new CommandDrawAttrInput( node.attribute( "text" ).value( ) );
            break;
        }

        default:
            std::cerr << "Procedure visualization encountered unsupported c-type "
                         "value! Skipping."
                      << std::endl;
            break;
    }

    elt->idNode =
     static_cast< unsigned int >( atoi( node.attribute( "id-node" ).value( ) ) );
    elt->idSub =
     static_cast< unsigned int >( atoi( node.attribute( "id-sub" ).value( ) ) );
    elt->x = atof( node.attribute( "x" ).value( ) );
    elt->y = atof( node.attribute( "y" ).value( ) );
    elt->w = atof( node.attribute( "w" ).value( ) );
    elt->h = atof( node.attribute( "h" ).value( ) );

    return elt;
}

unsigned int
 AssetMessaging::populateEditorLayout( CommandQueue & commandQueue,
                                       int pixelsW,
                                       int pixelsH ) const
{
    std::string uriEditorLayout = uriResBound;
    uriEditorLayout += "editor-layout";
    uriEditorLayout += "?pixels-w=" + std::to_string( pixelsW );
    uriEditorLayout += "&pixels-h=" + std::to_string( pixelsH );
    pugi::xml_document docResponse;

    requestGet( client, docResponse, uriEditorLayout.c_str( ) );
    // docResponse.save( std::cout );

    pugi::xpath_node xp_layout = docResponse.select_node( "//layout" );

    commandQueue.pixelsW = atof( xp_layout.node( ).attribute( "pixels-w" ).value( ) );
    commandQueue.pixelsH = atof( xp_layout.node( ).attribute( "pixels-h" ).value( ) );

    pugi::xpath_node_set xp_commands = docResponse.select_nodes( "//layout/command" );

    for ( pugi::xpath_node_set::const_iterator it = xp_commands.begin( );
          it != xp_commands.end( );
          ++it )
    {
        commandQueue.commands.emplace_back( generateCommand( it->node( ) ) );
    }

    return 0;
}

unsigned int
 AssetMessaging::populateCommandOptions( std::vector< std::string > & commandOptions,
                                         unsigned int idNode,
                                         unsigned int idSub ) const
{
    std::string uriOptions = uriResBound;
    uriOptions += "option";
    uriOptions += "?id-node=" + std::to_string( idNode );
    uriOptions += "&id-sub=" + std::to_string( idSub );
    pugi::xml_document docResponse;

    requestGet( client, docResponse, uriOptions.c_str( ) );

    pugi::xpath_node_set xp = docResponse.select_nodes( "//options/option" );

    for ( pugi::xpath_node_set::const_iterator it = xp.begin( ); it != xp.end( ); ++it )
    {
        commandOptions.emplace_back( it->node( ).attribute( "name" ).value( ) );
    }

    return 0;
}

unsigned int
 AssetMessaging::applyCommandOption( unsigned int idNode,
                                     unsigned int idSub,
                                     unsigned int idOpt ) const
{
    std::string uriOptions = uriResBound;
    uriOptions += "option";

    std::string idNodeStr = std::to_string( idNode );
    std::string idSubStr = std::to_string( idSub );
    std::string idOptStr = std::to_string( idOpt );

    pugi::xml_document docResponse;
    pugi::xml_document docRequest;
    pugi::xml_node node_action = docRequest.append_child( "action" );
    pugi::xml_attribute attr_action_name = node_action.append_attribute( "name" );
    attr_action_name.set_value( "apply-option" );
    pugi::xml_attribute attr_action_idNode = node_action.append_attribute( "id-node" );
    attr_action_idNode.set_value( idNodeStr.c_str( ) );
    pugi::xml_attribute attr_action_idSub = node_action.append_attribute( "id-sub" );
    attr_action_idSub.set_value( idSubStr.c_str( ) );
    pugi::xml_attribute attr_action_idOpt = node_action.append_attribute( "id-opt" );
    attr_action_idOpt.set_value( idOptStr.c_str( ) );

    requestPost( client, docResponse, uriOptions.c_str( ), docRequest );

    return 0;
}

}   // namespace mopr
