#include "client_ecl.h"

// NOTE: This is a CPP file for now, only to keep the build setup simple.
//       It is implemented with the assumption that it will later be a C file.

static cl_object
 getSymbol( const char * symName, cl_object pkg_l )
{
    cl_object strSym_l = ecl_make_constant_base_string( symName, -1 );
    int flags = 0;
    return ecl_find_symbol( strSym_l, pkg_l, &flags );
}

unsigned int
 Client_ECL_readLispFile( void * pLayer,
                          CommandQueue * queue,
                          const char * resolvedPath,
                          unsigned int callEnabled )
{
    cl_object hLayer_l = ecl_make_pointer( pLayer );
    cl_object hQueue_l = ecl_make_pointer( queue );
    cl_object pkgMoprExtUtil_l = ecl_find_package( "MOPR-EXT/UTIL" );
    cl_object symFn_l =
     getSymbol( "POPULATE-FROM-LISP-FILE-WITH-REPR", pkgMoprExtUtil_l );
    cl_object strFileName_l = ecl_make_constant_base_string( resolvedPath, -1 );
    cl_funcall(
     5, symFn_l, hLayer_l, hQueue_l, strFileName_l, callEnabled ? ECL_T : ECL_NIL );
    return 0;
}

unsigned int
 Client_ECL_destructCommandQueue( CommandQueue * queue )
{
    cl_object hQueue_l = ecl_make_pointer( queue );
    cl_object pkgMoprGuiRepr_l = ecl_find_package( "MOPR-GUI/REPR" );
    cl_object symFn_l = getSymbol( "DESTRUCT-COMMAND-QUEUE", pkgMoprGuiRepr_l );
    cl_funcall( 2, symFn_l, hQueue_l );
    return 0;
}

unsigned int
 Client_ECL_populateCommandOptions( CommandOptions * options,
                                    unsigned int id,
                                    unsigned int idSub )
{
    cl_object id_l = ecl_make_unsigned_integer( id );
    cl_object idSub_l = ecl_make_unsigned_integer( idSub );
    cl_object hOptions_l = ecl_make_pointer( options );
    cl_object pkgMoprGuiRepr_l = ecl_find_package( "MOPR-GUI/REPR" );
    cl_object symFn_l = getSymbol( "POPULATE-COMMAND-OPTIONS", pkgMoprGuiRepr_l );
    cl_funcall( 4, symFn_l, hOptions_l, id_l, idSub_l );
    return 0;
}

unsigned int
 Client_ECL_destructCommandOptions( CommandOptions * options )
{
    cl_object hOptions_l = ecl_make_pointer( options );
    cl_object pkgMoprGuiRepr_l = ecl_find_package( "MOPR-GUI/REPR" );
    cl_object symFn_l = getSymbol( "DESTRUCT-COMMAND-OPTIONS", pkgMoprGuiRepr_l );
    cl_funcall( 2, symFn_l, hOptions_l );
    return 0;
}

unsigned int
 Client_ECL_applyOption( unsigned int id, unsigned int idSub, unsigned int idOpt )
{
    cl_object id_l = ecl_make_unsigned_integer( id );
    cl_object idSub_l = ecl_make_unsigned_integer( idSub );
    cl_object idOpt_l = ecl_make_unsigned_integer( idOpt );
    cl_object pkgMoprGuiRepr_l = ecl_find_package( "MOPR-GUI/REPR" );
    cl_object symFn_l = getSymbol( "APPLY-COMMAND-OPTION", pkgMoprGuiRepr_l );
    cl_funcall( 4, symFn_l, id_l, idSub_l, idOpt_l );
    return 0;
}
