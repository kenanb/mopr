#ifndef MOPR_WRAP_USD_EXT_SCHEMA_INFO_H
#define MOPR_WRAP_USD_EXT_SCHEMA_INFO_H

// Wrap util includes.
#include "wrap/_base/ext/common.h"
#include "wrap/_base/ext/prolog.h"

#include "wrap/usd/ext/token.h"

// Generic includes.
#include "base/api.h"

#ifdef __cplusplus
extern "C"
{
#endif

    enum MoprSchemaKind
    {
        MoprSchemaKindInvalid,
        MoprSchemaKindAbstractBase,
        MoprSchemaKindAbstractTyped,
        MoprSchemaKindConcreteTyped,
        MoprSchemaKindNonAppliedAPI,
        MoprSchemaKindSingleApplyAPI,
        MoprSchemaKindMultipleApplyAPI
    };

    //
    // TF SCHEMA INFO WRAPPER
    //

    MOPR_DECLARE_HANDLE( MoprSchemaInfo )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprSchemaInfo, schema_info )

    /* Query */

    MOPR_API
    _Bool
     mopr_schema_info_is_empty_p( MoprSchemaInfo_ch this_ch );

    MOPR_API
    enum MoprSchemaKind
     mopr_schema_info_get_kind( MoprSchemaInfo_ch this_ch );

    MOPR_API
    unsigned int
     mopr_schema_info_get_version( MoprSchemaInfo_ch this_ch );

    MOPR_API
    void
     mopr_schema_info_get_identifier( MoprToken_h id_h, MoprSchemaInfo_ch this_ch );

    MOPR_API
    void
     mopr_schema_info_get_family( MoprToken_h family_h, MoprSchemaInfo_ch this_ch );

    //
    // API
    //

    /* Constructor */

    MOPR_API
    void
     mopr_schema_info_ctor( MoprSchemaInfo_h this_h );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_SCHEMA_INFO_H
