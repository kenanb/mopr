#ifndef MOPR_MAIN_MESSAGING_H
#define MOPR_MAIN_MESSAGING_H

#include "appEnvironment.h"

#include <string>
#include <vector>

namespace mopr
{

struct Messaging
{
    std::string uriEpW;
    std::string uriResW;
    std::string uriEpP;
    std::string uriResP;
    std::string uriEpPL;
    std::string uriEpA;
    std::string uriResA;
    std::string pLockState;
    std::string uriEpStaging;
    std::string uriEpWorking;
    std::string uriResBound;

    Messaging( );

    void
     debugPrint( );

    unsigned int
     initBackend( const std::string & workshopPath );

    unsigned int
     termBackend( );

    unsigned int
     bindStaging( );

    unsigned int
     initRepr( );

    unsigned int
     termRepr( );

    unsigned int
     initGenericWorkshopEndpoints( const AppEnvironment * appEnvironment );

    unsigned int
     initGenericProjectEndpoints( const AppEnvironment * appEnvironment );

    unsigned int
     acquireProject( );

    unsigned int
     releaseProject( );

    unsigned int
     populateEditorLayout( int pixelsW, int pixelsH );

    unsigned int
     populateCommandOptions( std::vector< std::string > & commandOptions,
                             unsigned int idNode,
                             unsigned int idSub );

    unsigned int
     applyCommandOption( unsigned int idNode, unsigned int idSub, unsigned int idOpt );
};

}   // namespace mopr

#endif   // MOPR_MAIN_MESSAGING_H
