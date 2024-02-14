#ifndef MOPR_CONFIG_H
#define MOPR_CONFIG_H

#include "base/api.h"

#include <set>
#include <string>

// Singleton responsible of paths etc.
struct Config
{
    static Config &
     GetInstance( )
    {
        static Config instance;
        return instance;
    }

    std::string
     FromDataset( std::string const & name, const char * ext ) const;

    void
     Print( ) const;

  private:
    std::string dataset;
    std::string projectRoot;

    Config( ) = default;
    Config( Config const & ) = delete;
    void
     operator=( Config const & ) = delete;

    friend int
     main( int argc, char ** argv );

    // Initialization is only accessible through main.
    bool
     Init( char const * projectRoot, char const * dataset );
};

#endif
