#include "timecode.h"

// Wrap internal includes.
#include "wrap/usd/box/timecode.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprTimecode, timecode )

/* Constructor */

void
 mopr_timecode_ctor( MoprTimecode_h this_h )
{
    this_h->d = { };
}

void
 mopr_timecode_ctor_default( MoprTimecode_h this_h )
{
    this_h->d = pxr::UsdTimeCode::Default( );
}

void
 mopr_timecode_ctor_earliest_time( MoprTimecode_h this_h )
{
    this_h->d = pxr::UsdTimeCode::EarliestTime( );
}

void
 mopr_timecode_ctor_double( MoprTimecode_h this_h, double time )
{
    this_h->d = { time };
}

/* Query */

MOPR_BOOL
 mopr_timecode_is_empty_p( MoprTimecode_ch this_ch )
{
    return this_ch->d.IsDefault( );
}

MOPR_BOOL
 mopr_timecode_is_default_p( MoprTimecode_ch this_ch )
{
    return this_ch->d.IsDefault( );
}

MOPR_BOOL
 mopr_timecode_is_earliest_time_p( MoprTimecode_ch this_ch )
{
    return this_ch->d.IsEarliestTime( );
}
