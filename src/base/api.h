#ifndef MOPR_API_H
#define MOPR_API_H

#if defined( MOPR_EXPORTS )
#    define MOPR_API __attribute__( ( visibility( "default" ) ) )
#else
#    define MOPR_API
#endif

#endif
