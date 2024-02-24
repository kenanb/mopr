#ifndef MOPR_WRAP_USD_EXT_SCHEMA_TYPE_SET_H
#define MOPR_WRAP_USD_EXT_SCHEMA_TYPE_SET_H

// Wrap external includes.
#include "schemaInfo.h"
#include "schemaTypeSet.h"

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
    // WRAPPER FOR USD SCHEMA TYPE SETS
    // GENERATED BY PLUGIN REGISTRY
    //

    MOPR_DECLARE_HANDLE( MoprSchemaTypeSet )

    MOPR_DECLARE_RAII_FUNCTIONS( MoprSchemaTypeSet, schema_type_set )

    /* Constructor */

    MOPR_API
    void
     mopr_schema_type_set_ctor( MoprSchemaTypeSet_h this_h );

    MOPR_API
    void
     mopr_schema_type_set_ctor_isa_derived( MoprSchemaTypeSet_h this_h );

    MOPR_API
    void
     mopr_schema_type_set_ctor_api_derived( MoprSchemaTypeSet_h this_h );

    /* Query */

    MOPR_API
    int
     mopr_schema_type_set_get_type_count( MoprSchemaTypeSet_ch this_ch );

    MOPR_API
    _Bool
     mopr_schema_type_set_get_schema_info( MoprSchemaInfo_h info_h,
                                           MoprSchemaTypeSet_ch this_ch,
                                           int index );

    MOPR_API
    _Bool
     mopr_schema_type_set_is_empty_p( MoprSchemaTypeSet_ch this_ch );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_WRAP_USD_EXT_SCHEMA_TYPE_SET_H