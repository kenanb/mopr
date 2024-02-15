#include "timecode.h"

// Wrap internal includes.
#include "wrap/usd/box/timecode.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprTimecode, timecode )

/* Constructor */

void
 mopr_timecode_ctor( MoprTimecode_h this_h )
{
    this_h->d = pxr::SdfTimeCode( );
}

void
 mopr_timecode_ctor_default( MoprTimecode_h this_h )
{
    this_h->SetDefault( );
}

void
 mopr_timecode_ctor_earliest_time( MoprTimecode_h this_h )
{
    this_h->SetEarliestTime( );
}

void
 mopr_timecode_ctor_double( MoprTimecode_h this_h, double time )
{
    this_h->d = pxr::SdfTimeCode( time );
}

/* Query */

_Bool
 mopr_timecode_is_empty_p( MoprTimecode_ch this_ch )
{
    return this_ch->IsDefault( );
}

_Bool
 mopr_timecode_is_default_p( MoprTimecode_ch this_ch )
{
    return this_ch->IsDefault( );
}

_Bool
 mopr_timecode_is_earliest_time_p( MoprTimecode_ch this_ch )
{
    return this_ch->IsEarliestTime( );
}
