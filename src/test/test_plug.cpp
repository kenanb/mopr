#include "config.h"
#include "routines/routines.h"
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

#include <fstream>
#include <iostream>

TEST_CASE( "USDS Output", "[usds]" )
{
    registerPlugins( );

    auto i = GENERATE( "00", "01", "02", "03", "04", "05", "06", "07" );
    const auto & config = Config::GetInstance( );
    const std::string & iFile = config.FromDataset( i, "lisp" );
    const std::string & oFile = config.FromDataset( i, "usda" );

    INFO( "Comparing files " << iFile << " -- " << oFile );

    std::string usdsAsUsda = generateUsdaString( iFile );
    std::string usdaAsUsda = exportToUsdaString( oFile );

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
    const std::string & oFile = config.FromDataset( i, "usda" );

    INFO( "Comparing files " << iFile << " -- " << oFile );

    std::string usdxAsUsda = exportToUsdaString( iFile );
    std::string usdaAsUsda = exportToUsdaString( oFile );
    REQUIRE( usdxAsUsda == usdaAsUsda );
}
