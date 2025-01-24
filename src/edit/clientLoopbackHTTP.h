#ifndef MOPR_MAIN_CLIENTLOOPBACKHTTP_H
#define MOPR_MAIN_CLIENTLOOPBACKHTTP_H

// Generic includes.
#include "base/api.h"

#include "client.h"

#include <string>

typedef void CURL;

namespace mopr
{

class MOPR_API ClientLoopbackHTTP final : public Client
{
    const std::string uriBase;
    CURL * curlHandle;

  public:
    ClientLoopbackHTTP( const char * wDirAbs, unsigned int port );

    virtual ~ClientLoopbackHTTP( );

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
};

}   // namespace mopr

#endif   // MOPR_MAIN_CLIENTLOOPBACKHTTP_H
