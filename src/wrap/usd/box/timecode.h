#ifndef MOPR_WRAP_USD_BOX_TIMECODE_H
#define MOPR_WRAP_USD_BOX_TIMECODE_H

#include "wrap/_base/box/generic.h"

#include "pxr/usd/sdf/timeCode.h"
#include "pxr/usd/usd/timeCode.h"

#include <limits>

struct MOPR_API MoprTimecode : public MoprGeneric
{
    // We use NaN as a sentinel for Empty,
    // as Usd uses for "default" timecode.
    //
    // We use the hardcoded static NaN value instead of
    // UsdTimeCode::Default().GetValue() because that
    // query generates an error for Default timecode.
    static constexpr const double defaultTimeValue =
     std::numeric_limits< double >::quiet_NaN( );

    pxr::SdfTimeCode d;

    MoprTimecode( ) : d( defaultTimeValue )
    {
    }

    void
     SetEarliestTime( )
    {
        d = pxr::UsdTimeCode::EarliestTime( ).GetValue( );
    }

    void
     SetDefault( )
    {
        d = defaultTimeValue;
    }

    bool
     IsDefault( ) const
    {
        return d == pxr::UsdTimeCode::Default( );
    }

    bool
     IsEarliestTime( ) const
    {
        return d == pxr::UsdTimeCode::EarliestTime( );
    }
};

#endif   // MOPR_WRAP_USD_BOX_TIMECODE_H
