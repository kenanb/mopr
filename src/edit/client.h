#ifndef MOPR_MAIN_CLIENT_H
#define MOPR_MAIN_CLIENT_H

#include <string>

namespace mopr
{

// NOTE: Most virtual functions in this API return unsigned int to
// communicate possible error. Zero means SUCCESS.

class Client
{
    const char * workshopPath;

  protected:
    Client( const char * workshopPath ) : workshopPath( workshopPath )
    {
    }

  public:
    virtual ~Client( ) = default;

    // Client class tree should be non-copyable.
    Client( const Client & ) = delete;

    Client &
     operator=( const Client & ) = delete;

  public:
    const char *
     getWorkshopPath( ) const
    {
        return workshopPath;
    }

    virtual unsigned int
     validate( ) const = 0;

    virtual unsigned int
     requestGet( char ** pResponse, const char * uri ) const = 0;

    virtual unsigned int
     requestPost( char ** pResponse,
                  const char * uri,
                  const char * requestBody ) const = 0;

    virtual unsigned int
     releaseResponse( char ** pResponse ) const = 0;
};

class ClientInProcess : public Client
{
  public:
    ClientInProcess( const char * workshopPath ) : Client( workshopPath )
    {
    }

    virtual ~ClientInProcess( ) override = default;

    virtual unsigned int
     execProcedure( void * pLayer, unsigned int callEnabled ) const = 0;
};

}   // namespace mopr

#endif   // MOPR_MAIN_CLIENT_H
