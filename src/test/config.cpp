#include "config.h"

#define PRINT_STR_MEMBER( X ) printf( "%-15s : %s\n", #X, this->X.c_str( ) )

bool
 Config::Init( char const * projectRoot, char const * dataset )
{
    this->projectRoot = projectRoot;
    this->dataset = dataset;
    return true;
}

std::string
 Config::FromDataset( std::string const & name, const char * ext ) const
{
    std::string path = this->projectRoot;
    path += '/';
    path += this->dataset;
    path += '/';
    path += name;
    path += '.';
    path += ext;
    return path;
}

void
 Config::Print( ) const
{
    printf(
     "\n# # # # # # # # # #"
     "\nTest configuration:\n" );
    PRINT_STR_MEMBER( projectRoot );
    PRINT_STR_MEMBER( dataset );
    printf( "# # # # # # # # # #\n" );
}
