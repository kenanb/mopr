#include "pxr/base/arch/attributes.h"

#include <ecl/ecl.h>

extern "C" void mopr_init_lisp_module_all( cl_object );

#include "test.inl"

// Dummy CLI args info passed to ECL init, which internally
// stores pointers to them, hence declared static.
//
// TODO [ 2024-01-26 ] : Check if this is safe.
#define ARGC_EMPTY 1
static char sEmptyPath[ 1 ];
static char * sEmptyArgv[ ARGC_EMPTY ];

static void
 initEmptyArgv( )
{
    sEmptyPath[ 0 ] = '\0';
    sEmptyArgv[ 0 ] = { sEmptyPath };
}

ARCH_CONSTRUCTOR( moprInit, 100 )
{
    initEmptyArgv( );

    // Initialize ECL.
    cl_boot( ARGC_EMPTY, sEmptyArgv );

    // Initialize the MOPR support library.
    ecl_init_module( NULL, mopr_init_lisp_module_all );

    // In case we need to move the ffi initialization to the boot code.
    // It is currently happening inside mopr-user.
    //
    // const cl_object pkg_mopr_ffi = ecl_find_package( "MOPR-USD/FFI" );
    // const cl_object str_ffi_init = ecl_make_constant_base_string( "INIT", -1 );
    // int sym_ffi_init_if = 0;
    // const cl_object sym_ffi_init =
    //  ecl_find_symbol( str_ffi_init, pkg_mopr_ffi, &sym_ffi_init_if );
    // cl_funcall( 1, sym_ffi_init );

    // Run some interpreter tests.
    // runExamples( );
}

ARCH_DESTRUCTOR( moprFini, 100 )
{
    // Shutdown ECL.
    cl_shutdown( );
}
