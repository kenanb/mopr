#include "appConfig.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include "pxr/base/js/json.h"

#pragma GCC diagnostic pop

#include <fstream>
#include <iostream>
#include <vector>

namespace mopr
{

// Fallback values.
AppConfig::AppConfig( )
    // Font settings:
    : fontDefault( "" )
    , fontHeading( "" )
    , fontBaseSize( 14 )
    // Screen settings:
    , screenW( 1024 )
    , screenH( 768 )
    // Render and scene settings:
    , renderer( )
    , complexity( 1.0 )
    , enableFrameAll( true )
    , enablePurposeGuide( true )
    , enablePurposeProxy( true )
    , enablePurposeRender( true )
    , enableLightingPlaceholder( true )
    , enableLightingScene( false )
    , enableLightingCamera( true )
    , enableSceneMaterials( true )
    , viewTranslate( { 0.0, 0.0, 0.0 } )
    , viewRotate( { 0.0, 0.0 } )
{
}

template < typename T >
static bool
 getJsValue( const pxr::JsObject & jsObj, T & location, const char * key )
{
    auto valPair{ jsObj.find( key ) };
    if ( valPair != jsObj.end( ) )
    {
        pxr::JsValue const & val{ valPair->second };
        if ( val.Is< T >( ) )
        {
            location = val.Get< T >( );
            return true;
        }
    }

    return false;
}

template < typename T >
static bool
 getJsArrayValue( const pxr::JsObject & jsObj,
                  std::vector< T > & location,
                  const char * key )
{
    auto valPair{ jsObj.find( key ) };
    if ( valPair != jsObj.end( ) )
    {
        pxr::JsValue const & val{ valPair->second };
        if ( val.IsArrayOf< T >( ) )
        {
            location = val.GetArrayOf< T >( );
            return true;
        }
    }

    return false;
}

bool
 AppConfig::Init( const std::string & appConfigAbsPath )
{
    bool result = false;
    pxr::JsParseError jsonError;
    std::ifstream jsonFile;
    jsonFile.open( appConfigAbsPath );
    pxr::JsValue const & jsonRoot{ pxr::JsParseStream( jsonFile, &jsonError ) };
    if ( jsonRoot.IsObject( ) )
    {
        result = true;
        pxr::JsObject const & jsObj = jsonRoot.GetJsObject( );
        getJsValue< std::string >( jsObj, this->renderer, "renderer" );
        getJsValue< std::string >( jsObj, this->fontDefault, "font-default" );
        getJsValue< std::string >( jsObj, this->fontHeading, "font-heading" );
        getJsValue< int >( jsObj, this->fontBaseSize, "font-base-size" );
        getJsValue< int >( jsObj, this->screenW, "screen-width" );
        getJsValue< int >( jsObj, this->screenH, "screen-height" );
        getJsValue< double >( jsObj, this->complexity, "complexity" );
        getJsValue< bool >( jsObj, this->enableFrameAll, "frame-all" );
        getJsValue< bool >( jsObj, this->enablePurposeGuide, "purpose-guide" );
        getJsValue< bool >( jsObj, this->enablePurposeProxy, "purpose-proxy" );
        getJsValue< bool >( jsObj, this->enablePurposeRender, "purpose-render" );
        getJsValue< bool >(
         jsObj, this->enableLightingPlaceholder, "lighting-placeholder" );
        getJsValue< bool >( jsObj, this->enableLightingScene, "lighting-scene" );
        getJsValue< bool >( jsObj, this->enableLightingCamera, "lighting-camera" );
        getJsValue< bool >( jsObj, this->enableSceneMaterials, "scene-materials" );
        getJsArrayValue< double >( jsObj, this->viewTranslate, "view-translate" );
        getJsArrayValue< double >( jsObj, this->viewRotate, "view-rotate" );
    }

    jsonFile.close( );

    return result;
}

}   // namespace mopr
