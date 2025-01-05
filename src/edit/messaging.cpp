#include "messaging.h"

#include "client_ecl.h"

#include "pugixml.hpp"

#include <iostream>
#include <sstream>

namespace mopr
{

Messaging::Messaging( )
    : uriEpW( )
    , uriResW( )
    , uriEpP( )
    , uriResP( )
    , uriEpPL( )
    , uriEpA( )
    , uriResA( )
    , pLockState( )
    , uriEpStaging( )
    , uriEpWorking( )
    , uriResBound( )
{
}

void
 Messaging::debugPrint( )
{
    std::cout << "\nWorkshop endpoint URI  : " << uriEpW
              << "\nWorkshop resource URI  : " << uriResW
              << "\nProject endpoint URI   : " << uriEpP
              << "\nProject resource URI   : " << uriResP
              << "\nProj lock endpoint URI : " << uriEpPL
              << "\nAsset endpoint URI     : " << uriEpA
              << "\nAsset resource URI     : " << uriResA
              << "\nStaging endpoint URI   : " << uriEpStaging
              << "\nWorking endpoint URI   : " << uriEpWorking
              << "\nBound resource URI     : " << uriResBound << std::endl;
}

unsigned int
 Messaging::initBackend( const std::string & workshopPath )
{
    return Client_ECL_initBackend( workshopPath.c_str( ) );
}

unsigned int
 Messaging::termBackend( )
{
    return Client_ECL_termBackend( );
}

static unsigned int
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

static unsigned int
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

static std::string
 requestGetAndSelectUri( const char * get, const char * select )
{
    pugi::xml_document doc;
    requestGet( doc, get );
    pugi::xpath_node xp = doc.select_node( select );

    std::string result;
    if ( xp ) result = xp.node( ).attribute( "uri" ).value( );
    return result;
}

static std::string
 locateEndpoint( const char * uriResource, const char * endpoint )
{
    std::string select = "//endpoints/endpoint[@name='";
    select += endpoint;
    select += "']";
    return requestGetAndSelectUri( uriResource, select.c_str( ) );
}

static std::string
 locateResourceWorkshop( const std::string & uriEndpointWorkshop )
{
    return requestGetAndSelectUri( uriEndpointWorkshop.c_str( ), "//workshop" );
}

static std::string
 locateResourceProject( const std::string & uriEndpointProject,
                        const AppEnvironment * appEnvironment )
{
    std::string selection = "//projects/project[@path='";
    selection += appEnvironment->getProjectPath( );
    selection += "']";
    return requestGetAndSelectUri( uriEndpointProject.c_str( ), selection.c_str( ) );
}

static std::string
 manageProjectLock( const std::string & uriResourceProject, bool acquire )
{
    pugi::xml_document docResponse;
    pugi::xml_document docRequest;

    pugi::xml_node node_action = docRequest.append_child( "action" );
    pugi::xml_attribute attr_action_name = node_action.append_attribute( "name" );
    attr_action_name.set_value( acquire ? "acquire" : "release" );

    requestPost( docResponse, uriResourceProject.c_str( ), docRequest );

    pugi::xpath_node xp = docResponse.select_node( "//project-lock" );

    std::string result;
    if ( xp ) result = xp.node( ).attribute( "state" ).value( );
    return result;
}

static std::string
 locateResourceAsset( const std::string & uriEndpointAsset,
                      const AppEnvironment * appEnvironment )
{
    std::string selection = "//assets/asset[@path='";
    selection += appEnvironment->getAssetPath( );
    selection += "']";
    return requestGetAndSelectUri( uriEndpointAsset.c_str( ), selection.c_str( ) );
}

unsigned int
 Messaging::initGenericWorkshopEndpoints( const AppEnvironment * appEnvironment )
{
    uriEpW = locateEndpoint( "/", "workshop" );
    if ( uriEpW.empty( ) ) return 0;

    uriResW = locateResourceWorkshop( uriEpW );
    if ( uriResW.empty( ) ) return 0;

    uriEpP = locateEndpoint( uriResW.c_str( ), "project" );
    if ( uriEpP.empty( ) ) return 0;

    uriResP = locateResourceProject( uriEpP, appEnvironment );
    if ( uriResP.empty( ) ) return 0;

    uriEpPL = locateEndpoint( uriResP.c_str( ), "lock" );
    if ( uriEpPL.empty( ) ) return 0;

    return 0;
}

unsigned int
 Messaging::initGenericProjectEndpoints( const AppEnvironment * appEnvironment )
{
    uriEpA = locateEndpoint( uriResP.c_str( ), "asset" );
    if ( uriEpA.empty( ) ) return 0;

    uriResA = locateResourceAsset( uriEpA, appEnvironment );
    if ( uriResA.empty( ) ) return 0;

    uriEpStaging = locateEndpoint( uriResA.c_str( ), "staging" );
    if ( uriEpStaging.empty( ) ) return 0;

    uriEpWorking = locateEndpoint( uriResA.c_str( ), "working" );
    if ( uriEpWorking.empty( ) ) return 0;

    return 0;
}

unsigned int
 Messaging::bindStaging( )
{
    pugi::xml_document docResponse;
    pugi::xml_document docRequest;

    pugi::xml_node node_action = docRequest.append_child( "action" );
    pugi::xml_attribute attr_action_name = node_action.append_attribute( "name" );
    attr_action_name.set_value( "bind" );

    requestPost( docResponse, uriEpStaging.c_str( ), docRequest );

    pugi::xpath_node xp = docResponse.select_node( "//asset" );

    uriResBound = xp.node( ).attribute( "uri" ).value( );

    std::string result;
    if ( xp ) result = xp.node( ).attribute( "path" ).value( );
    std::cout << "Loaded asset path: " << result << std::endl;

    return 0;
}

unsigned int
 Messaging::initRepr( )
{
    pugi::xml_document docResponse;
    pugi::xml_document docRequest;

    pugi::xml_node node_action = docRequest.append_child( "action" );
    pugi::xml_attribute attr_action_name = node_action.append_attribute( "name" );
    attr_action_name.set_value( "init-repr" );

    requestPost( docResponse, uriResBound.c_str( ), docRequest );

    return 0;
}

unsigned int
 Messaging::termRepr( )
{
    pugi::xml_document docResponse;
    pugi::xml_document docRequest;

    pugi::xml_node node_action = docRequest.append_child( "action" );
    pugi::xml_attribute attr_action_name = node_action.append_attribute( "name" );
    attr_action_name.set_value( "term-repr" );

    requestPost( docResponse, uriResBound.c_str( ), docRequest );

    return 0;
}

unsigned int
 Messaging::acquireProject( )
{
    pLockState = manageProjectLock( uriEpPL, true );
    if ( pLockState != "acquired" ) return 0;

    return 0;
}

unsigned int
 Messaging::releaseProject( )
{
    pLockState = manageProjectLock( uriEpPL, false );
    if ( pLockState != "released" ) return 0;

    return 0;
}

unsigned int
 Messaging::populateEditorLayout( int pixelsW, int pixelsH )
{
    std::string uriEditorLayout = uriResBound;
    uriEditorLayout += "editor-layout";
    uriEditorLayout += "?pixels-w=" + std::to_string( pixelsW );
    uriEditorLayout += "&pixels-h=" + std::to_string( pixelsH );
    pugi::xml_document docResponse;

    requestGet( docResponse, uriEditorLayout.c_str( ) );
    docResponse.save( std::cout );

    return 0;
}

unsigned int
 Messaging::populateCommandOptions( std::vector< std::string > & commandOptions,
                                    unsigned int idNode,
                                    unsigned int idSub )
{
    std::string uriOptions = uriResBound;
    uriOptions += "option";
    uriOptions += "?id-node=" + std::to_string( idNode );
    uriOptions += "&id-sub=" + std::to_string( idSub );
    pugi::xml_document docResponse;

    requestGet( docResponse, uriOptions.c_str( ) );

    pugi::xpath_node_set xp = docResponse.select_nodes( "//options/option" );

    for ( pugi::xpath_node_set::const_iterator it = xp.begin( ); it != xp.end( ); ++it )
    {
        commandOptions.emplace_back( it->node( ).attribute( "name" ).value( ) );
    }

    return 0;
}

unsigned int
 Messaging::applyCommandOption( unsigned int idNode,
                                unsigned int idSub,
                                unsigned int idOpt )
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

    requestPost( docResponse, uriOptions.c_str( ), docRequest );

    return 0;
}

}   // namespace mopr
