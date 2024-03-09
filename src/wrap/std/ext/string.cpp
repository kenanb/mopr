#include "string.h"

// Wrap internal includes.
#include "wrap/std/box/string.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprString, string )

/* Constructor */

void
 mopr_string_ctor( MoprString_h this_h )
{
    this_h->d = std::string( );
}

void
 mopr_string_ctor_cstr( MoprString_h this_h, char const * cstr )
{
    this_h->d = std::string( cstr );
}

/* Query */

MOPR_BOOL
 mopr_string_is_empty_p( MoprString_ch this_ch )
{
    return this_ch->d.empty( );
}

char const *
 mopr_string_cstr( MoprString_ch this_ch )
{
    return this_ch->d.c_str( );
}
