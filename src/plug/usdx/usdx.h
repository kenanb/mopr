#ifndef MOPR_USDX_FORMAT_H
#define MOPR_USDX_FORMAT_H

#include "ffns.h"

#include "base/mopr.h"

#include "pxr/base/tf/staticTokens.h"
#include "pxr/pxr.h"
#include "pxr/usd/sdf/fileFormat.h"

#include <iosfwd>
#include <iostream>
#include <string>

MOPR_NAMESPACE_OPEN_SCOPE

MOPR_DECLARE_WEAK_AND_REF_PTRS( MoprUsdxFileFormat );

class MoprUsdxFileFormat : public pxr::SdfFileFormat
{
  public:
    virtual bool
     CanRead( const std::string & file ) const override
    {
        return true;
    }

    virtual bool
     Read( pxr::SdfLayer * layer,
           const std::string & resolvedPath,
           bool metadataOnly ) const override;

  protected:
    MOPR_SDF_FILE_FORMAT_FACTORY_ACCESS;

    MoprUsdxFileFormat( );

    virtual ~MoprUsdxFileFormat( ) = default;
};

MOPR_NAMESPACE_CLOSE_SCOPE

#define MOPR_USDX_FILE_FORMAT_TOKENS                                                     \
    ( ( Id, "usdx" ) )( ( Version, "1.0" ) )( ( Target, "usd" ) )

PXR_NAMESPACE_OPEN_SCOPE

TF_DECLARE_PUBLIC_TOKENS( MoprUsdxFileFormatTokens, MOPR_USDX_FILE_FORMAT_TOKENS );

PXR_NAMESPACE_CLOSE_SCOPE

#endif   // MOPR_USDX_FORMAT_H
