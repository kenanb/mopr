#include "clientLoopbackHTTP.h"

#include <curl/curl.h>

#include <string.h>

#define MOPR_HTTP_CONTENT_TYPE "application/xml; charset=utf-8"
#define MOPR_HTTP_CONTENT_TYPE_SIZE 30

// Response is often received in chunks. So it gets accumulated.
// Lifetime of the data extends the lifetime of container. Once fully
// accumulated, data pointer is passed and freed separately.
struct ResponseContainer
{
    size_t size;   // Sentinel excluded from size.
    char * data;   // Allocation : size + 1 (sentinel)
};

static size_t
 cbAccumulateResponse( void * contentsReceived,   // Not null-terminated.
                       size_t contentSize,        // Documented as "always 1".
                       size_t nofContents,        // Size of data.
                       void * pRcVoid             // User data.
 )
{
    size_t realSize = contentSize * nofContents;   // Effective size of data.

    struct ResponseContainer * pRc = ( struct ResponseContainer * ) pRcVoid;
    void * newData = realloc( pRc->data, pRc->size + realSize + 1 );

    if ( newData == nullptr )
    {
        printf( "Realloc failed while accumulating CURL query response!\n" );
        return 0;
    }

    pRc->data = static_cast< char * >( newData );
    memcpy( &( pRc->data[ pRc->size ] ), contentsReceived, realSize );
    pRc->size += realSize;
    pRc->data[ pRc->size ] = 0;   // Write sentinel.

    return realSize;
}

static std::string
 generateUriBase( unsigned int port )
{
    if ( port < 1024 )
    {
        printf( "Ports in Well Known Ports list is not suitable! Exiting.\n" );
        exit( -1 );
    }
    else if ( port > 65535 )
    {
        printf( "Port number is out of range! Exiting.\n" );
        exit( -1 );
    }

    const std::string schemeAndHost = "http://127.0.0.1";
    std::string schemeAndAuthority = schemeAndHost;
    schemeAndAuthority += ":";
    schemeAndAuthority += std::to_string( port );
    return schemeAndAuthority;
}

namespace mopr
{

ClientLoopbackHTTP::ClientLoopbackHTTP( const char * wDirAbs, unsigned int port )
    : Client( wDirAbs ), uriBase( generateUriBase( port ) ), curlHandle( nullptr )
{
    curl_global_init( CURL_GLOBAL_NOTHING );   // We currently just use HTTP.
    curlHandle = curl_easy_init( );
}

ClientLoopbackHTTP::~ClientLoopbackHTTP( )
{
    if ( curlHandle ) curl_easy_cleanup( curlHandle );
    curl_global_cleanup( );
}

unsigned int
 ClientLoopbackHTTP::validate( ) const
{
    // TODO

    return 0;
}

unsigned int
 ClientLoopbackHTTP::requestGet( char ** pResponse, const char * uri ) const
{
    if ( !curlHandle ) return 1;

    std::string fullUri = uriBase;
    fullUri += uri;

    if ( CURLE_OK != curl_easy_setopt( curlHandle, CURLOPT_URL, fullUri.c_str( ) ) )
    {
        printf( "Failed to set CURL option! Exiting.\n" );
        exit( -1 );
    }

    // NOTE: Consider setting CURLOPT_USERAGENT.

    // if ( CURLE_OK != curl_easy_setopt( curlHandle, CURLOPT_VERBOSE, 1L ) )
    // {
    //     printf( "Failed to set CURL option! Exiting.\n" );
    //     exit( -1 );
    // }

    if ( CURLE_OK
         != curl_easy_setopt( curlHandle, CURLOPT_WRITEFUNCTION, cbAccumulateResponse ) )
    {
        printf( "Failed to set CURL option! Exiting.\n" );
        exit( -1 );
    }

    struct ResponseContainer rc;

    rc.data = static_cast< char * >( malloc( 1 ) );   // Alloc sentinel only.
    rc.size = 0;                                      // Size except sentinel.
    rc.data[ rc.size ] = 0;                           // Write sentinel.

    if ( CURLE_OK != curl_easy_setopt( curlHandle, CURLOPT_WRITEDATA, ( void * ) &rc ) )
    {
        printf( "Failed to set CURL option! Exiting.\n" );
        exit( -1 );
    }

    // Not necessary if the handle was reset, but kept to ensure correct state.
    if ( CURLE_OK != curl_easy_setopt( curlHandle, CURLOPT_HTTPGET, 1L ) )
    {
        printf( "Failed to set CURL option! Exiting.\n" );
        exit( -1 );
    }

    if ( CURLE_OK != curl_easy_perform( curlHandle ) )
    {
        printf( "CURL query returned an error! Exiting.\n" );
        exit( -1 );
    }

    long responseCode;
    if ( CURLE_OK
         != curl_easy_getinfo( curlHandle, CURLINFO_RESPONSE_CODE, &responseCode ) )
    {
        printf( "Failed to extract the response code of CURL query! Exiting.\n" );
        exit( -1 );
    }
    else if ( 200 != responseCode )
    {
        printf( "CURL query succeded but response is not an HTTP 200 OK! Exiting.\n" );
        exit( -1 );
    }

    // Assign accumulated value as response.
    *pResponse = rc.data;

    struct curl_header * cnType;
    if ( CURLHE_OK
         != curl_easy_header( curlHandle, "Content-Type", 0, CURLH_HEADER, -1, &cnType ) )
    {
        printf( "Can't read CURL response header! Exiting.\n" );
        exit( -1 );
    }
    else if ( strncmp( cnType->value, MOPR_HTTP_CONTENT_TYPE, MOPR_HTTP_CONTENT_TYPE_SIZE ) )
    {
        printf( "CURL query returned an unexpected content type! Exiting.\n" );
        exit( -1 );
    }

    curl_easy_reset( curlHandle );
    return 0;
}

unsigned int
 ClientLoopbackHTTP::requestPost( char ** pResponse,
                                  const char * uri,
                                  const char * requestBody ) const
{
    if ( !curlHandle ) return 1;

    std::string fullUri = uriBase;
    fullUri += uri;

    if ( CURLE_OK != curl_easy_setopt( curlHandle, CURLOPT_URL, fullUri.c_str( ) ) )
    {
        printf( "Failed to set CURL option! Exiting.\n" );
        exit( -1 );
    }

    // NOTE: Consider setting CURLOPT_USERAGENT.

    // if ( CURLE_OK != curl_easy_setopt( curlHandle, CURLOPT_VERBOSE, 1L ) )
    // {
    //     printf( "Failed to set CURL option! Exiting.\n" );
    //     exit( -1 );
    // }

    if ( CURLE_OK
         != curl_easy_setopt( curlHandle, CURLOPT_WRITEFUNCTION, cbAccumulateResponse ) )
    {
        printf( "Failed to set CURL option! Exiting.\n" );
        exit( -1 );
    }

    struct ResponseContainer rc;

    rc.data = static_cast< char * >( malloc( 1 ) );   // Alloc sentinel only.
    rc.size = 0;                                      // Size except sentinel.
    rc.data[ rc.size ] = 0;                           // Write sentinel.

    if ( CURLE_OK != curl_easy_setopt( curlHandle, CURLOPT_WRITEDATA, ( void * ) &rc ) )
    {
        printf( "Failed to set CURL option! Exiting.\n" );
        exit( -1 );
    }

    if ( CURLE_OK != curl_easy_setopt( curlHandle, CURLOPT_POST, 1L ) )
    {
        printf( "Failed to set CURL option! Exiting.\n" );
        exit( -1 );
    }

    if ( CURLE_OK != curl_easy_setopt( curlHandle, CURLOPT_POSTFIELDS, requestBody ) )
    {
        printf( "Failed to set CURL option! Exiting.\n" );
        exit( -1 );
    }

    struct curl_slist * slHeader = NULL;
    slHeader = curl_slist_append( slHeader, "Content-Type: " MOPR_HTTP_CONTENT_TYPE );

    if ( CURLE_OK != curl_easy_setopt( curlHandle, CURLOPT_HTTPHEADER, slHeader ) )
    {
        printf( "Failed to set CURL option! Exiting.\n" );
        exit( -1 );
    }

    if ( CURLE_OK != curl_easy_perform( curlHandle ) )
    {
        printf( "CURL query returned an error! Exiting.\n" );
        exit( -1 );
    }

    curl_slist_free_all( slHeader );

    long responseCode;
    if ( CURLE_OK
         != curl_easy_getinfo( curlHandle, CURLINFO_RESPONSE_CODE, &responseCode ) )
    {
        printf( "Failed to extract the response code of CURL query! Exiting.\n" );
        exit( -1 );
    }
    else if ( 200 != responseCode )
    {
        printf( "CURL query succeded but response is not an HTTP 200 OK! Exiting.\n" );
        exit( -1 );
    }

    // Assign accumulated value as response.
    *pResponse = rc.data;

    struct curl_header * cnType;
    if ( CURLHE_OK
         != curl_easy_header( curlHandle, "Content-Type", 0, CURLH_HEADER, -1, &cnType ) )
    {
        printf( "Can't read CURL response header! Exiting.\n" );
        exit( -1 );
    }
    else if ( strncmp( cnType->value, MOPR_HTTP_CONTENT_TYPE, MOPR_HTTP_CONTENT_TYPE_SIZE ) )
    {
        printf( "CURL query returned an unexpected content type! Exiting.\n" );
        exit( -1 );
    }

    curl_easy_reset( curlHandle );
    return 0;
}

unsigned int
 ClientLoopbackHTTP::releaseResponse( char ** pResponse ) const
{
    free( *pResponse );
    *pResponse = nullptr;

    return 0;
}

}   // namespace mopr
