#ifndef MOPR_WRAP__BASE_BOX_UTIL_H
#define MOPR_WRAP__BASE_BOX_UTIL_H

#include <cstdlib>
#include <errno.h>
#include <error.h>

#define MOPR_ASSERT_FATAL( cond, errnum, ... )                                           \
    do                                                                                   \
    {                                                                                    \
        if ( !( cond ) ) error( EXIT_FAILURE, errnum, __VA_ARGS__ );                     \
    } while ( 0 )

#define MOPR_ASSERT_FATAL_INTERNAL( cond, fname )                                        \
    MOPR_ASSERT_FATAL( cond, 0, "%s: INTERNAL ERROR %d", fname, __LINE__ )

static void *
 moprMalloc( size_t n )
{
    void * p = malloc( n );
    MOPR_ASSERT_FATAL( p, 0, "Out of memory!" );
    return p;
}

#endif   // MOPR_WRAP__BASE_BOX_UTIL_H
