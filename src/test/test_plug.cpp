#include "config.h"
#include "routines/routines.h"
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <fstream>
#include <iostream>

TEST_CASE( "USDS Output", "[usds]" )
{
    registerPlugins( );

    const std::string i = GENERATE( "00",
                                    "01",
                                    "02",
                                    "03",
                                    "04",
                                    "05",
                                    "06",
                                    "07",
                                    "10",
                                    "11_grid",
                                    "12_grid",
                                    "13_grid",
                                    "14_grid",
                                    "15_grid" );

    const auto & config = Config::GetInstance( );
    const std::string & iFile = config.FromDataset( i, "lisp" );
    const std::string::size_type pos = i.find( '_' );
    const std::string & rFile =
     config.FromDataset( pos != std::string::npos ? i.substr( 0, pos ) : i, "usda" );

    INFO( "Comparing files " << iFile << " -- " << rFile );

    std::string usdsAsUsda = generateUsdaString( iFile );
    std::string usdaAsUsda = exportToUsdaString( rFile );

    // Remove first line, as our export produces "sdf" header, not "usd".
    usdsAsUsda.erase( 0, usdsAsUsda.find( "\n" ) + 1 );
    usdaAsUsda.erase( 0, usdaAsUsda.find( "\n" ) + 1 );

    REQUIRE( usdsAsUsda == usdaAsUsda );
}

TEST_CASE( "USDS Callable Output", "[usds]" )
{
    registerPlugins( );

    const std::string i = GENERATE( "08_call",
                                    "09_call",
                                    "11_call",
                                    "12_call",
                                    "13_call",
                                    "14_call",
                                    "15_call",
                                    "15_call-gen" );

    const auto & config = Config::GetInstance( );
    const std::string & iFile = config.FromDataset( i, "lisp" );
    const std::string::size_type pos = i.find( '_' );
    const std::string & rFile =
     config.FromDataset( pos != std::string::npos ? i.substr( 0, pos ) : i, "usda" );

    INFO( "Comparing files " << iFile << " -- " << rFile );

    std::string usdsAsUsda = generateUsdaString( iFile, /* callEnabled = */ true );
    std::string usdaAsUsda = exportToUsdaString( rFile );

    // Remove first line, as our export produces "sdf" header, not "usd".
    usdsAsUsda.erase( 0, usdsAsUsda.find( "\n" ) + 1 );
    usdaAsUsda.erase( 0, usdaAsUsda.find( "\n" ) + 1 );

    REQUIRE( usdsAsUsda == usdaAsUsda );
}

TEST_CASE( "USDX File Format Output", "[usdx]" )
{
    auto i = GENERATE( "00" );
    const auto & config = Config::GetInstance( );
    const std::string & iFile = config.FromDataset( i, "usdx" );
    const std::string & rFile = config.FromDataset( i, "usda" );

    INFO( "Comparing files " << iFile << " -- " << rFile );

    std::string usdxAsUsda = exportToUsdaString( iFile );
    std::string usdaAsUsda = exportToUsdaString( rFile );
    REQUIRE( usdxAsUsda == usdaAsUsda );
}
