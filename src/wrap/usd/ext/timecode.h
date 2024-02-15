#ifndef MOPR_WRAP_USD_EXT_TIMECODE_H
#define MOPR_WRAP_USD_EXT_TIMECODE_H

// Wrap util includes.
#include "wrap/_base/ext/common.h"
#include "wrap/_base/ext/prolog.h"

// Generic includes.
#include "base/api.h"

#ifdef __cplusplus
extern "C"
{
#endif

    //
    // USD TIMECODE WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprTimecode )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprTimecode, timecode )

    /* Query */

    // NOTE : We use Default as the sentinel value to also mean "empty".
    MOPR_API
    _Bool
     mopr_timecode_is_empty_p( MoprTimecode_ch this_ch );

    MOPR_API
    _Bool
     mopr_timecode_is_default_p( MoprTimecode_ch this_ch );

    MOPR_API
    _Bool
     mopr_timecode_is_earliest_time_p( MoprTimecode_ch this_ch );

    //
    // API
    //

    /* Constructor */

    MOPR_API
    void
     mopr_timecode_ctor( MoprTimecode_h this_h );

    MOPR_API
    void
     mopr_timecode_ctor_default( MoprTimecode_h this_h );

    MOPR_API
    void
     mopr_timecode_ctor_earliest_time( MoprTimecode_h this_h );

    MOPR_API
    void
     mopr_timecode_ctor_double( MoprTimecode_h this_h, double time );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_TIMECODE_H
