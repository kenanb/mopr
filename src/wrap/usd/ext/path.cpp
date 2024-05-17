#include "path.h"

// Wrap std internal includes.
#include "wrap/std/box/string.h"

// Wrap internal includes.
#include "wrap/usd/box/path.h"
#include "wrap/usd/box/token.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprPath, path )

/* Constructor */

void
 mopr_path_ctor( MoprPath_h this_h )
{
    this_h->d = pxr::SdfPath( );
}

void
 mopr_path_ctor_cstr( MoprPath_h this_h, char const * path_cstr )
{
    const std::string & path{ path_cstr };
    this_h->d = pxr::SdfPath( path );
}

/* Query */

MOPR_BOOL
 mopr_path_is_empty_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsEmpty( );
}

int
 mopr_path_get_path_element_count( MoprPath_ch this_ch )
{
    return this_ch->d.GetPathElementCount( );
}

MOPR_BOOL
 mopr_path_is_absolute_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsAbsolutePath( );
}

MOPR_BOOL
 mopr_path_is_absolute_root_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsAbsoluteRootPath( );
}

MOPR_BOOL
 mopr_path_is_prim_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsPrimPath( );
}

MOPR_BOOL
 mopr_path_is_absolute_root_or_prim_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsAbsoluteRootOrPrimPath( );
}

MOPR_BOOL
 mopr_path_is_root_prim_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsRootPrimPath( );
}

MOPR_BOOL
 mopr_path_is_property_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsPropertyPath( );
}

MOPR_BOOL
 mopr_path_is_prim_property_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsPrimPropertyPath( );
}

MOPR_BOOL
 mopr_path_is_namespaced_property_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsNamespacedPropertyPath( );
}

MOPR_BOOL
 mopr_path_is_prim_variant_selection_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsPrimVariantSelectionPath( );
}

MOPR_BOOL
 mopr_path_is_prim_or_prim_variant_selection_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsPrimOrPrimVariantSelectionPath( );
}

MOPR_BOOL
 mopr_path_contains_prim_variant_selection_p( MoprPath_ch this_ch )
{
    return this_ch->d.ContainsPrimVariantSelection( );
}

MOPR_BOOL
 mopr_path_contains_property_elements_p( MoprPath_ch this_ch )
{
    return this_ch->d.ContainsPropertyElements( );
}

MOPR_BOOL
 mopr_path_contains_target_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.ContainsTargetPath( );
}

MOPR_BOOL
 mopr_path_is_relational_attribute_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsRelationalAttributePath( );
}

MOPR_BOOL
 mopr_path_is_target_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsTargetPath( );
}

MOPR_BOOL
 mopr_path_is_mapper_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsMapperPath( );
}

MOPR_BOOL
 mopr_path_is_mapper_arg_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsMapperArgPath( );
}

MOPR_BOOL
 mopr_path_is_expression_path_p( MoprPath_ch this_ch )
{
    return this_ch->d.IsExpressionPath( );
}

void
 mopr_path_get_as_string( MoprString_h string_h, MoprPath_ch this_ch )
{
    string_h->d = this_ch->d.GetAsString( );
}

void
 mopr_path_get_as_token( MoprToken_h token_h, MoprPath_ch this_ch )
{
    token_h->d = this_ch->d.GetAsToken( );
}
