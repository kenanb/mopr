#include "appEnvironment.h"
#include "common.h"
#include "defs.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include "pxr/base/arch/fileSystem.h"
#include "pxr/base/arch/systemInfo.h"

#pragma GCC diagnostic pop

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <libgen.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

namespace mopr
{

AppEnvironment::AppEnvironment( int argc, char * argv[] )
    : action( ActionRun )
    , appRoot( )
    , appConfigPath( NULL )
    , workshopPath( NULL )
    , projectPath( NULL )
    , assetPath( NULL )
    , frameFirst( 0.0 )
    , frameLast( 100.0 )
    , portNumber( 0 )   // 0 means "use in-process backend".
{
    // Extract app root.
    //

    std::string exePath = pxr::ArchGetExecutablePath( );

    // Get the app root path from the executable path.
    {
        // Note :: This assumes the executable is located at the app root.  But as long as the
        // default files are at the same location as the executable, all calculated relative paths
        // should successfully resolve.
        char * appRoot_ = ( char * ) xmalloc( exePath.size( ) + 1 );
        strcpy( appRoot_, exePath.c_str( ) );
        this->appRoot = dirname( appRoot_ );
        free( appRoot_ );
    }

    // Parse command line arguments.
    //

    int opt = 0;

    this->action = ActionRun;

    while ( ( opt = getopt( argc, argv, "c:w:p:a:P:f:l:h" ) ) != -1 )
    {
        switch ( opt )
        {
            case 'c':   // absolute appConfig path
                this->appConfigPath = optarg;
                break;

            case 'w':   // workshop path
                this->workshopPath = optarg;
                break;

            case 'p':   // workshop-relative project path
                this->projectPath = optarg;
                break;

            case 'a':   // project-relative asset path
                this->assetPath = optarg;
                break;

            case 'P':   // port number
            {
                const std::string portArg = optarg;
                unsigned long port = std::stoul( portArg );
                if ( port < 1024 )
                {
                    printf(
                     "Ports in Well Known Ports list is not suitable! Exiting.\n" );
                    exit( -1 );
                }
                else if ( port > 65535 )
                {
                    printf( "Port number is out of range! Exiting.\n" );
                    exit( -1 );
                }
                else
                {
                    this->portNumber = port;
                }
                break;
            }

            case 'f':   // first frame
            {
                const std::string frameFirstArg = optarg;
                this->frameFirst = std::stoi( frameFirstArg );
                break;
            }

            case 'l':   // last frame
            {
                const std::string frameLastArg = optarg;
                this->frameLast = std::stoi( frameLastArg );
                break;
            }

            case 'h':   // help
                this->action = ActionHelpSuccess;
                break;

            default:
                this->action = ActionHelpFailure;
                break;
        }
    }

    if ( !( this->workshopPath && this->projectPath && this->assetPath ) )
    {
        this->action = ActionHelpFailure;
    }
}

std::string
 AppEnvironment::resolveCwdRelativePath( char const * path ) const
{
    return pxr::ArchAbsPath( path );
}

std::string
 AppEnvironment::resolveAppRelativePath( char const * path ) const
{
    if ( path[ 0 ] == '/' )
    {
        return { path };
    }
    else
    {
        std::string absPath = this->appRoot;
        absPath += "/";
        absPath += path;
        // Using ArchAbsPath to ensure the resolved path is canonical.
        return pxr::ArchAbsPath( absPath );
    }
}

std::string
 AppEnvironment::getResolvedAppConfigPath( ) const
{
    if ( this->appConfigPath )
    {
        return this->resolveCwdRelativePath( this->appConfigPath );
    }
    else
    {
        return this->resolveAppRelativePath( "res/defaults.json" );
    }
}

std::string
 AppEnvironment::getResolvedWorkshopPath( ) const
{
    if ( this->workshopPath )
    {
        std::string ret = this->resolveCwdRelativePath( this->workshopPath );
        if ( ret.back( ) != '/' )
        {
            // Trailing slash is important to ensure successful resource query.
            ret += '/';
        }
        return ret;
    }
    else
    {
        printf( "Workshop path was not provided! Exiting!\n" );
        exit( -1 );
    }
}

std::string
 AppEnvironment::getProjectPath( ) const
{
    if ( this->projectPath )
    {
        std::string ret = this->projectPath;
        if ( ret.back( ) != '/' )
        {
            // Trailing slash is important to ensure successful resource query.
            ret += '/';
        }
        return ret;
    }
    else
    {
        return "";
    }
}

std::string
 AppEnvironment::getAssetPath( ) const
{
    if ( this->assetPath )
    {
        return this->assetPath;
    }
    else
    {
        return "";
    }
}

unsigned int
 AppEnvironment::getPortNumber( ) const
{
    return this->portNumber;
}

}   // namespace mopr
