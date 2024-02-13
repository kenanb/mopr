// -*- Mode: C++ -*-

static cl_object
 example00( )
{
    return ecl_current_package( );
}

static cl_object
 example01( )
{
    return cl_eval( ecl_read_from_cstring( "(mopr-user::test-mopr)" ) );
}

static cl_object
 example02( )
{
    const cl_object pkgUsds_l = ecl_find_package( "MOPR-USER" );
    const cl_object strTestUsds_l = ecl_make_constant_base_string( "TEST-MOPR", -1 );
    int symTestUsdsIf = 0;
    const cl_object symTestUsds_l =
     ecl_find_symbol( strTestUsds_l, pkgUsds_l, &symTestUsdsIf );
    return cl_funcall( 1, symTestUsds_l );
}

static cl_object
 example03( )
{
    const cl_object kwDefspec_l = ecl_make_symbol( "DEFSPEC", "USDS" );
    return ecl_symbol_package( kwDefspec_l );
}

static cl_object
 example04( )
{
    const cl_object pkgUsds_l = ecl_find_package( "MOPR" );
    const cl_object strTestWrap_l = ecl_make_constant_base_string( "TEST-WRAP-STD", -1 );
    int symTestWrapIf = 0;
    const cl_object symTestWrap_l =
     ecl_find_symbol( strTestWrap_l, pkgUsds_l, &symTestWrapIf );
    return cl_funcall( 1, symTestWrap_l );
}

static cl_object
 example05( )
{
    const cl_object pkgUsds_l = ecl_find_package( "MOPR" );
    const cl_object strTestWrap_l = ecl_make_constant_base_string( "TEST-WRAP-USD", -1 );
    int symTestWrapIf = 0;
    const cl_object symTestWrap_l =
     ecl_find_symbol( strTestWrap_l, pkgUsds_l, &symTestWrapIf );
    return cl_funcall( 1, symTestWrap_l );
}

void
 run_examples( )
{
    ecl_print( example00( ), ECL_T );
    ecl_print( example01( ), ECL_T );
    ecl_print( example02( ), ECL_T );
    ecl_print( example03( ), ECL_T );
    ecl_print( example04( ), ECL_T );
    ecl_print( example05( ), ECL_T );
}
