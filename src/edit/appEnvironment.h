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
    char const * resourcePath;
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
     getResolvedWorkshopPath( ) const;

    std::string
     getProjectPath( ) const;

    std::string
     getResourcePath( ) const;
};

}   // namespace mopr

#endif   // MOPR_MAIN_APPENVIRONMENT_H
