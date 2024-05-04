#include "common.h"

#include <stdlib.h>

namespace mopr
{

void *
 xmalloc( size_t n )
{
    void * p = malloc( n );
    ASSERT_FATAL( p, 0, "Out of memory!" );
    return p;
}

}   // namespace mopr
