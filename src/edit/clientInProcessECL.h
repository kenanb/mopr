#ifndef MOPR_MAIN_CLIENTINPROCESSECL_H
#define MOPR_MAIN_CLIENTINPROCESSECL_H

// Generic includes.
#include "base/api.h"

#include "client.h"

namespace mopr
{

class MOPR_API ClientInProcessECL final
    : public ClientInProcess
{
  public:
    ClientInProcessECL( const char * wDirAbs );

    virtual ~ClientInProcessECL( );

    virtual unsigned int
     validate( ) const override;

    virtual unsigned int
     requestGet( char ** pResponse, const char * uri ) const override;

    virtual unsigned int
     requestPost( char ** pResponse,
                  const char * uri,
                  const char * requestBody ) const override;

    virtual unsigned int
     releaseResponse( char ** pResponse ) const override;

    virtual unsigned int
     execProcedure( void * pLayer, unsigned int callEnabled ) const override;
};

}   // namespace mopr

#endif   // MOPR_MAIN_CLIENTINPROCESSECL_H
