#ifndef MOPR_WRAP_USD_EXT_PROPERTY_DEFINITION_H
#define MOPR_WRAP_USD_EXT_PROPERTY_DEFINITION_H

// Wrap external includes.
#include "token.h"
#include "valueTypeName.h"

// Wrap util includes.
#include "wrap/_base/ext/common.h"
#include "wrap/_base/ext/prolog.h"

#include "wrap/usd/ext/variability.h"

// Generic includes.
#include "base/api.h"

#ifdef __cplusplus
extern "C"
{
#endif

    //
    // USD PROPERTY DEFINITION WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprPropertyDefinition )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprPropertyDefinition, property_definition )

    /* Query */

    MOPR_API
    _Bool
     mopr_property_definition_is_empty_p( MoprPropertyDefinition_ch this_ch );

    //
    // API
    //

    /* Constructor */

    MOPR_API
    void
     mopr_property_definition_ctor( MoprPropertyDefinition_h this_h );

    MOPR_API
    _Bool
     mopr_property_definition_get_name( MoprToken_h token_h,
                                        MoprPropertyDefinition_ch this_ch );

    MOPR_API
    _Bool
     mopr_property_definition_is_attribute_p( MoprPropertyDefinition_ch this_ch );

    MOPR_API
    _Bool
     mopr_property_definition_is_relationship_p( MoprPropertyDefinition_ch this_ch );

    MOPR_API
    _Bool
     mopr_property_definition_attribute_get_type_name(
      MoprValueTypeName_h value_tyne_name_h, MoprPropertyDefinition_ch this_ch );

    MOPR_API
    _Bool
     mopr_property_definition_attribute_get_type_name_token(
      MoprToken_h token_h, MoprPropertyDefinition_ch this_ch );

    MOPR_API
    enum MoprPropertyVariability
     mopr_property_definition_get_variability( MoprPropertyDefinition_ch this_ch );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_PROPERTY_DEFINITION_H
