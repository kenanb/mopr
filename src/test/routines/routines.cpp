#include "routines.h"

#include "wrap/usd/box/layer.h"
#include "wrap/usd/box/stage.h"
#include "wrap/usd/external.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#include "pxr/base/plug/plugin.h"
#include "pxr/base/plug/registry.h"
#include "pxr/usd/sdf/layer.h"
#include "pxr/usd/usd/stageCache.h"
#include "pxr/usd/usd/stageCacheContext.h"
#pragma GCC diagnostic pop

#include <fstream>
#include <iostream>

#include "base/mopr.h"

#include "wrap/usd/box/layer.h"

#include <ecl/ecl.h>

PXR_NAMESPACE_USING_DIRECTIVE

void
 registerPlugins( )
{
    PlugPluginPtr usdx = PlugRegistry::GetInstance( ).GetPluginWithName( "Usdx" );
    if ( !usdx->IsLoaded( ) )
    {
        usdx->Load( );
    }
}

static cl_object
 getSymbol( const char * symName, cl_object pkg_l )
{
    cl_object strSym_l = ecl_make_constant_base_string( symName, -1 );
    int flags = 0;
    return ecl_find_symbol( strSym_l, pkg_l, &flags );
}

static bool
 readLispFile( SdfLayerRefPtr layer,
               const std::string & resolvedPath,
               bool callEnabled = false )
{
    MoprLayer sLayer;
    sLayer.SetRefPtr( layer );
    cl_object hLayer_l = ecl_make_pointer( &sLayer );
    cl_object pkgMoprExtUtil_l = ecl_find_package( "MOPR-EXT/UTIL" );
    cl_object symFn_l = getSymbol( "POPULATE-FROM-LISP-FILE", pkgMoprExtUtil_l );
    cl_object strFileName_l = ecl_make_constant_base_string( resolvedPath.c_str( ), -1 );
    cl_funcall( 4, symFn_l, hLayer_l, strFileName_l, callEnabled ? ECL_T : ECL_NIL );

    if ( !layer ) return false;

    return true;
}

std::string
 generateUsdaString( const std::string & src, bool callEnabled )
{
    std::string x;
    bool res = false;

    SdfLayerRefPtr lyr = SdfLayer::CreateAnonymous( src );
    int result = readLispFile( lyr, src, callEnabled );
    ( void ) result;

    if ( lyr )
    {
        res = lyr->ExportToString( &x );
    }

    if ( !res )
    {
        printf( "ERROR!\n" );
        return x;
    }

    return x;
}

std::string
 exportToUsdaString( const std::string & src )
{
    std::string x;
    bool res = false;

    SdfLayerRefPtr lyr = SdfLayer::FindOrOpen( src );

    if ( lyr )
    {
        res = lyr->ExportToString( &x );
    }

    if ( !res )
    {
        printf( "ERROR!\n" );
        return x;
    }

    return x;
}
