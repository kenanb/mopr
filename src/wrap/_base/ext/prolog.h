#ifndef MOPR_WRAP__BASE_EXT_PROLOG_H
#define MOPR_WRAP__BASE_EXT_PROLOG_H

#ifdef __cplusplus

#    include <stddef.h>

// A MOPR_BOOL macro is used for bool, because:
//
// - We avoid introducing stdbool.h (and using bool) in C, to avoid
// - extra set of definitions showing up in parsed API files.
//
// - Including stdbool.h in C++ to get _Bool recognized doesn't seem
// - to work for clang++ with standard C++. TODO: Check extensions.
#    define MOPR_BOOL bool
#else
#    define MOPR_BOOL _Bool
#endif

#endif   // MOPR_WRAP__BASE_PROLOG_H
