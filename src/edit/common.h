#ifndef MOPR_MAIN_COMMON_H
#define MOPR_MAIN_COMMON_H

#include <assert.h>
#include <errno.h>
#include <error.h>
#include <unistd.h>

#define ASSERT_FATAL( cond, errnum, ... )                                                \
    do                                                                                   \
    {                                                                                    \
        if ( !( cond ) ) error( EXIT_FAILURE, errnum, __VA_ARGS__ );                     \
    } while ( 0 )

namespace mopr
{

extern void *
 xmalloc( size_t n );

}   // namespace mopr

#endif   // MOPR_MAIN_COMMON_H
