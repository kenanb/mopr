#ifndef MOPR_MAIN_CLIENT_ECL_H
#define MOPR_MAIN_CLIENT_ECL_H

#include <ecl/ecl.h>

#include "repr/command.h"

// Generic includes.
#include "base/api.h"

#ifdef __cplusplus
extern "C"
{
#endif

    MOPR_API unsigned int
     Client_ECL_populateFromLispFile( void * pLayer,
                                      const char * resolvedPath,
                                      unsigned int callEnabled );

    MOPR_API unsigned int
     Client_ECL_initRepr( );

    MOPR_API unsigned int
     Client_ECL_termRepr( );

    MOPR_API unsigned int
     Client_ECL_populateCommandQueue( CommandQueue * queue );

    MOPR_API unsigned int
     Client_ECL_destructCommandQueue( CommandQueue * queue );

    MOPR_API unsigned int
     Client_ECL_populateCommandOptions( CommandOptions * options,
                                        unsigned int id,
                                        unsigned int idSub );

    MOPR_API unsigned int
     Client_ECL_destructCommandOptions( CommandOptions * options );

    MOPR_API unsigned int
     Client_ECL_applyOption( unsigned int id, unsigned int idSub, unsigned int idOpt );

#ifdef __cplusplus
}
#endif

#endif   // MOPR_MAIN_CLIENT_ECL_H
