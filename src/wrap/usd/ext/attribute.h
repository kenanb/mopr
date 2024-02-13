#ifndef MOPR_WRAP_USD_EXT_ATTRIBUTE_H
#define MOPR_WRAP_USD_EXT_ATTRIBUTE_H

// Wrap util includes.
#include "wrap/_base/ext/common.h"
#include "wrap/_base/ext/prolog.h"

#include "wrap/usd/ext/value.h"

// Generic includes.
#include "base/api.h"

#ifdef __cplusplus
extern "C"
{
#endif

    enum MoprAttributeVariability
    {
        MoprAttributeVariabilityVarying,
        MoprAttributeVariabilityUniform,
    };

    //
    // TF ATTRIBUTE WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprAttribute )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprAttribute, attribute )

    /* Query */

    MOPR_API
    _Bool
     mopr_attribute_is_empty_p( MoprAttribute_ch this_ch );

    //
    // API
    //

    /* Constructor */

    MOPR_API
    void
     mopr_attribute_ctor( MoprAttribute_h this_h );

    MOPR_API
    _Bool
     mopr_attribute_set_value( MoprAttribute_ch this_ch, MoprValue_ch value_ch );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_ATTRIBUTE_H
