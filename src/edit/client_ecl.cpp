#include "client_ecl.h"

// NOTE: This is a CPP file for now, only to keep the build setup simple.
//       It is implemented with the assumption that it will later be a C file.

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
