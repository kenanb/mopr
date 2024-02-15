#ifndef MOPR_WRAP_USD_BOX_TIMECODE_H
#define MOPR_WRAP_USD_BOX_TIMECODE_H

#include "wrap/_base/box/generic.h"

#include "pxr/usd/usd/timeCode.h"

#include <limits>

struct MOPR_API MoprTimecode : public MoprGeneric
{
    pxr::UsdTimeCode d;

    MoprTimecode( ) : d( pxr::UsdTimeCode::Default( ) )
    {
    }
};

#endif   // MOPR_WRAP_USD_BOX_TIMECODE_H
