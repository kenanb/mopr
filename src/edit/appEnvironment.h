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
    char const * workshopPath;
    char const * projectPath;
    char const * assetPath;
    double frameFirst;
    double frameLast;
    unsigned int portNumber;

    AppEnvironment( int argc, char * argv[] );

    bool
     statPath( char const * path, int * mode ) const;

    std::string
     resolveCwdRelativePath( char const * path ) const;

    std::string
     resolveAppRelativePath( char const * path ) const;

    std::string
     getResolvedAppConfigPath( ) const;

    std::string
     getResolvedWorkshopPath( ) const;

    std::string
     getProjectPath( ) const;

    std::string
     getAssetPath( ) const;

    unsigned int
     getPortNumber( ) const;
};

}   // namespace mopr

#endif   // MOPR_MAIN_APPENVIRONMENT_H
