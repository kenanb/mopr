#ifndef MOPR_MAIN_APPENVIRONMENT_H
#define MOPR_MAIN_APPENVIRONMENT_H

#include <string>

namespace mopr
{

struct AppEnvironment
{
    enum Action
    {
        ActionRun,
        ActionHelpSuccess,
        ActionHelpFailure,
    };

    Action action;
    std::string appRoot;
    char const * appConfigPath;
    char const * inputPath;
    char const * camera;
    double frameFirst;
    double frameLast;

    AppEnvironment( int argc, char * argv[] );

    std::string
     resolveCwdRelativePath( char const * path ) const;

    std::string
     resolveAppRelativePath( char const * path ) const;

    std::string
     getResolvedAppConfigPath( ) const;

    std::string
     getResolvedInputPath( ) const;
};

}   // namespace mopr

#endif   // MOPR_MAIN_APPENVIRONMENT_H
