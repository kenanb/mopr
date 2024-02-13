#ifndef MOPR_WRAP__BASE_EXT_PROLOG_H
#define MOPR_WRAP__BASE_EXT_PROLOG_H

#ifdef __cplusplus

// Bringing in stdbool in C++ to get _Bool recognized.  We avoid
// introducing it into C header just to avoid extra definitions
// showing up in the parsed file.
#    include <stdbool.h>
#    include <stddef.h>

#endif

#endif   // MOPR_WRAP__BASE_PROLOG_H
