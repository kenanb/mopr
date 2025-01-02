#include "client_ecl.h"

// NOTE: This is a CPP file for now, only to keep the build setup simple.
//       It is implemented with the assumption that it will later be a C file.

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

unsigned int
 Client_ECL_populateFromLispFile( void * pLayer,
                                  const char * resolvedPath,
                                  unsigned int callEnabled )
{
    cl_object symFnRead_l = getSymbol( "MOPR-EXT/UTIL", "POPULATE-FROM-LISP-FILE" );

    cl_object hLayer_l = ecl_make_pointer( pLayer );
    cl_object strFileName_l = ecl_make_constant_base_string( resolvedPath, -1 );

    cl_object rootNode_l =
     cl_funcall( 4, symFnRead_l, hLayer_l, strFileName_l, callEnabled ? ECL_T : ECL_NIL );

    cl_object symFnBind_l = getSymbol( "MOPR-VIZ/REPR", "BIND-REPR" );

    cl_funcall( 2, symFnBind_l, rootNode_l );

    return 0;
}

unsigned int
 Client_ECL_initRepr( )
{
    cl_object symFn_l = getSymbol( "MOPR-VIZ/REPR", "INIT-REPR" );

    cl_funcall( 1, symFn_l );
    return 0;
}

unsigned int
 Client_ECL_termRepr( )
{
    cl_object symFn_l = getSymbol( "MOPR-VIZ/REPR", "TERM-REPR" );

    cl_funcall( 1, symFn_l );
    return 0;
}

unsigned int
 Client_ECL_populateCommandQueue( CommandQueue * queue )
{
    cl_object symFn_l = getSymbol( "MOPR-VIZ/REPR", "POPULATE-COMMAND-QUEUE" );

    cl_object hQueue_l = ecl_make_pointer( queue );

    cl_funcall( 2, symFn_l, hQueue_l );
    return 0;
}

unsigned int
 Client_ECL_destructCommandQueue( CommandQueue * queue )
{
    cl_object symFn_l = getSymbol( "MOPR-VIZ/REPR", "DESTRUCT-COMMAND-QUEUE" );

    cl_object hQueue_l = ecl_make_pointer( queue );

    cl_funcall( 2, symFn_l, hQueue_l );
    return 0;
}

unsigned int
 Client_ECL_populateCommandOptions( CommandOptions * options,
                                    unsigned int id,
                                    unsigned int idSub )
{
    cl_object symFn_l = getSymbol( "MOPR-VIZ/REPR", "POPULATE-COMMAND-OPTIONS" );

    cl_object id_l = ecl_make_unsigned_integer( id );
    cl_object idSub_l = ecl_make_unsigned_integer( idSub );
    cl_object hOptions_l = ecl_make_pointer( options );

    cl_funcall( 4, symFn_l, hOptions_l, id_l, idSub_l );
    return 0;
}

unsigned int
 Client_ECL_destructCommandOptions( CommandOptions * options )
{
    cl_object symFn_l = getSymbol( "MOPR-VIZ/REPR", "DESTRUCT-COMMAND-OPTIONS" );

    cl_object hOptions_l = ecl_make_pointer( options );

    cl_funcall( 2, symFn_l, hOptions_l );
    return 0;
}

unsigned int
 Client_ECL_applyOption( unsigned int id, unsigned int idSub, unsigned int idOpt )
{
    cl_object symFn_l = getSymbol( "MOPR-VIZ/REPR", "APPLY-COMMAND-OPTION" );

    cl_object id_l = ecl_make_unsigned_integer( id );
    cl_object idSub_l = ecl_make_unsigned_integer( idSub );
    cl_object idOpt_l = ecl_make_unsigned_integer( idOpt );

    cl_funcall( 4, symFn_l, id_l, idSub_l, idOpt_l );
    return 0;
}

unsigned int
 Client_ECL_initBackend( const char * wDirAbs )
{
    cl_object symFnInitBackend_l = getSymbol( "MOPR-SRV", "IN-PROCESS-BACKEND-INIT" );

    cl_object strDir_l = ecl_make_constant_base_string( wDirAbs, -1 );

    cl_funcall( 2, symFnInitBackend_l, strDir_l );

    return 0;
}

unsigned int
 Client_ECL_termBackend( )
{
    cl_object symFnTermBackend_l = getSymbol( "MOPR-SRV", "IN-PROCESS-BACKEND-TERM" );

    cl_funcall( 1, symFnTermBackend_l );

    return 0;
}

unsigned int
 Client_ECL_requestGet( const char ** pResponse, const char * uri )
{
    cl_object symFnRequest_l =
     getSymbol( "MOPR-SRV", "IN-PROCESS-BACKEND-HANDLE-GET-REQUEST" );

    cl_object hpResponse_l = ecl_make_pointer( ( void * ) pResponse );
    cl_object strURI_l = ecl_make_constant_base_string( uri, -1 );

    cl_funcall( 3, symFnRequest_l, hpResponse_l, strURI_l );

    return 0;
}

unsigned int
 Client_ECL_requestPost( const char ** pResponse,
                         const char * uri,
                         const char * requestBody )
{
    cl_object symFnRequest_l =
     getSymbol( "MOPR-SRV", "IN-PROCESS-BACKEND-HANDLE-POST-REQUEST" );

    cl_object hpResponse_l = ecl_make_pointer( ( void * ) pResponse );
    cl_object strURI_l = ecl_make_constant_base_string( uri, -1 );
    cl_object requestBody_l = ecl_make_constant_base_string( requestBody, -1 );

    cl_funcall( 4, symFnRequest_l, hpResponse_l, strURI_l, requestBody_l );

    return 0;
}

unsigned int
 Client_ECL_releaseResponse( const char ** pResponse )
{
    cl_object symFnRelease_l =
     getSymbol( "MOPR-SRV", "IN-PROCESS-BACKEND-RELEASE-RESPONSE" );

    cl_object hpResponse_l = ecl_make_pointer( ( void * ) pResponse );

    cl_funcall( 2, symFnRelease_l, hpResponse_l );

    return 0;
}
