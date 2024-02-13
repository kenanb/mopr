#include "token.h"

// Wrap std internal includes.
#include "wrap/std/box/string.h"

// Wrap internal includes.
#include "wrap/usd/box/token.h"

MOPR_DEFINE_RAII_FUNCTIONS( MoprToken, token )

/* Constructor */

void
 mopr_token_ctor( MoprToken_h this_h )
{
    this_h->d = pxr::TfToken( );
}

void
 mopr_token_ctor_cstr( MoprToken_h this_h, char const * token_cstr )
{
    const std::string & token{ token_cstr };
    this_h->d = pxr::TfToken( token );
}

void
 mopr_token_ctor_string( MoprToken_h this_h, MoprString_ch string_ch )
{
    this_h->d = pxr::TfToken( string_ch->d );
}

/* Query */

_Bool
 mopr_token_is_empty_p( MoprToken_ch this_ch )
{
    return this_ch->d.IsEmpty( );
}
