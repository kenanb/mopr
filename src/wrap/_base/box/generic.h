#ifndef MOPR_WRAP__BASE_BOX_GENERIC_H
#define MOPR_WRAP__BASE_BOX_GENERIC_H

#include "base/api.h"

#include "util.h"

#include <iostream>
#include <new>

struct MOPR_API MoprGeneric
{
    /*
    MoprGeneric( )
    {
        std::cerr << "Ctor called for MoprGeneric!" << std::endl;
    }

    ~MoprGeneric( )
    {
        std::cerr << "Dtor called for MoprGeneric!" << std::endl;
    }
    */

    inline __attribute__( ( always_inline ) ) void *
     operator new( size_t size )
    {
        // std::cerr << "Op new called for MoprGeneric!" << std::endl;
        void * p = moprMalloc( size );
        return p;
    }

    inline __attribute__( ( always_inline ) ) void
     operator delete( void * p )
    {
        // std::cerr << "Op delete called for MoprGeneric!" << std::endl;
        free( p );
    }
};

#endif   // MOPR_WRAP__BASE_BOX_GENERIC_H
