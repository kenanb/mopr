#include "usdx.h"

#include "pxr/pxr.h"

#include "pxr/usd/sdf/layer.h"

// #include "wrap/usd/box/layer.h"

#include <ecl/ecl.h>

#include <algorithm>
#include <fstream>
#include <vector>

PXR_NAMESPACE_OPEN_SCOPE

TF_DECLARE_WEAK_AND_REF_PTRS( SdfLayer );

TF_DEFINE_PUBLIC_TOKENS( MoprUsdxFileFormatTokens, MOPR_USDX_FILE_FORMAT_TOKENS );

PXR_NAMESPACE_CLOSE_SCOPE

PXR_NAMESPACE_USING_DIRECTIVE

MOPR_NAMESPACE_OPEN_SCOPE

TF_REGISTRY_FUNCTION( TfType )
{
    SDF_DEFINE_FILE_FORMAT( MoprUsdxFileFormat, SdfFileFormat );
}

MoprUsdxFileFormat::MoprUsdxFileFormat( )
    : SdfFileFormat( MoprUsdxFileFormatTokens->Id,
                     MoprUsdxFileFormatTokens->Version,
                     MoprUsdxFileFormatTokens->Target,
                     MoprUsdxFileFormatTokens->Id )
{
}

bool
 MoprUsdxFileFormat::Read( SdfLayer * layer,
                           const std::string & resolvedPath,
                           bool metadataOnly ) const
{
    SdfLayerRefPtr tmpLayer = SdfLayer::CreateAnonymous( resolvedPath );

    // MoprLayer sLayer;
    // sLayer.SetRefPtr( tmpLayer );
    // cl_object hLayer_l = ecl_make_pointer( &sLayer );

    const cl_object pkgMoprUser_l = ecl_find_package( "MOPR-USER" );
    const cl_object strTestMopr_l = ecl_make_constant_base_string( "TEST-MOPR", -1 );
    int symTestMoprIf = 0;
    const cl_object symTestMopr_l =
     ecl_find_symbol( strTestMopr_l, pkgMoprUser_l, &symTestMoprIf );
    cl_funcall( 1, symTestMopr_l );

    return false;
}

MOPR_NAMESPACE_CLOSE_SCOPE
