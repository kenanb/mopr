#include "config.h"
#include <catch2/catch_session.hpp>

int
 main( int argc, char * argv[] )
{
    int result;

    Catch::Session session;

    std::string abs_res_dir;

    using namespace Catch::Clara;

    auto cli = session.cli( )
               | Opt( abs_res_dir, "directory" )[ "-R" ][ "--resource-directory" ](
                "Test resourse root directory." );

    session.cli( cli );

    result = session.applyCommandLine( argc, argv );

    if ( result ) return result;

    if ( abs_res_dir.empty( ) ) return EXIT_FAILURE;

    auto & config = Config::GetInstance( );

    config.Init( abs_res_dir.c_str( ), "src/data/test" );

    config.Print( );

    result = session.run( );

    return result;
}
