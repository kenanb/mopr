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
    , resourcePath( NULL )
    , camera( NULL )
    , frameFirst( 0.0 )
    , frameLast( 100.0 )
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

    while ( ( opt = getopt( argc, argv, "c:w:p:r:C:f:l:h" ) ) != -1 )
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

            case 'r':   // project-relative resource path
                this->resourcePath = optarg;
                break;

            case 'C':   // scene camera
                this->camera = optarg;
                break;

            case 'f':   // first frame
            {
                std::string frameFirst = optarg;
                this->frameFirst = std::stoi( optarg );
                break;
            }

            case 'l':   // last frame
            {
                std::string frameLast = optarg;
                this->frameLast = std::stoi( optarg );
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

    if ( !( this->workshopPath && this->projectPath && this->resourcePath ) )
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
        return this->resolveCwdRelativePath( this->workshopPath );
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
        return this->projectPath;
    }
    else
    {
        printf( "Project path was not provided! Exiting!\n" );
        exit( -1 );
    }
}

std::string
 AppEnvironment::getResourcePath( ) const
{
    if ( this->resourcePath )
    {
        return this->resourcePath;
    }
    else
    {
        printf( "Resource path was not provided! Exiting!\n" );
        exit( -1 );
    }
}

}   // namespace mopr
