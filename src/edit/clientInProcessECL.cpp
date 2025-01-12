#include "clientInProcessECL.h"

#include <ecl/ecl.h>

// TODO : Consider using ecl_make_simple_base_string instead of constant variant
//        in this API. The "simple" variant creates a simple-base-string with a
//        fresh string allocation, while "constant" variant points at the data
//        of C string.

static cl_object
 getSymbol( const char * pkgName, const char * symName )
{
    cl_object pkg_l = ecl_find_package( pkgName );
    cl_object strSym_l = ecl_make_constant_base_string( symName, -1 );
    int flags = 0;
    return ecl_find_symbol( strSym_l, pkg_l, &flags );
}

namespace mopr
{

ClientInProcessECL::ClientInProcessECL( const char * wDirAbs )
    : ClientInProcess( wDirAbs )
{
    cl_object symFnInitBackend_l = getSymbol( "MOPR-SRV/IN-PROCESS", "BACKEND-INIT" );

    cl_object strDir_l = ecl_make_constant_base_string( wDirAbs, -1 );

    cl_funcall( 2, symFnInitBackend_l, strDir_l );
}

ClientInProcessECL::~ClientInProcessECL( )
{
    cl_object symFnTermBackend_l = getSymbol( "MOPR-SRV/IN-PROCESS", "BACKEND-TERM" );

    cl_funcall( 1, symFnTermBackend_l );
}

unsigned int
 ClientInProcessECL::validate( ) const
{
    return 0;
}

unsigned int
 ClientInProcessECL::requestGet( char ** pResponse, const char * uri ) const
{
    cl_object symFnRequest_l = getSymbol( "MOPR-SRV/IN-PROCESS", "HANDLE-GET-REQUEST" );

    cl_object hpResponse_l = ecl_make_pointer( ( void * ) pResponse );
    cl_object strURI_l = ecl_make_constant_base_string( uri, -1 );

    cl_funcall( 3, symFnRequest_l, hpResponse_l, strURI_l );

    return 0;
}

unsigned int
 ClientInProcessECL::requestPost( char ** pResponse,
                                  const char * uri,
                                  const char * requestBody ) const
{
    cl_object symFnRequest_l = getSymbol( "MOPR-SRV/IN-PROCESS", "HANDLE-POST-REQUEST" );

    cl_object hpResponse_l = ecl_make_pointer( ( void * ) pResponse );
    cl_object strURI_l = ecl_make_constant_base_string( uri, -1 );
    cl_object requestBody_l = ecl_make_constant_base_string( requestBody, -1 );

    cl_funcall( 4, symFnRequest_l, hpResponse_l, strURI_l, requestBody_l );

    return 0;
}

unsigned int
 ClientInProcessECL::releaseResponse( char ** pResponse ) const
{
    cl_object symFnRelease_l = getSymbol( "MOPR-SRV/IN-PROCESS", "RELEASE-RESPONSE" );

    cl_object hpResponse_l = ecl_make_pointer( ( void * ) pResponse );

    cl_funcall( 2, symFnRelease_l, hpResponse_l );

    return 0;
}

unsigned int
 ClientInProcessECL::execProcedure( void * pLayer, unsigned int callEnabled ) const
{
    cl_object symFnExec_l = getSymbol( "MOPR-SRV/IN-PROCESS", "EXEC-PROCEDURE" );

    cl_object hLayer_l = ecl_make_pointer( pLayer );

    cl_funcall( 3, symFnExec_l, hLayer_l, callEnabled ? ECL_T : ECL_NIL );

    return 0;
}

}   // namespace mopr
