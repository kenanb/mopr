#ifndef MOPR_MAIN_MESSAGING_H
#define MOPR_MAIN_MESSAGING_H

#include "appEnvironment.h"
#include "client.h"
#include "procedureViz.h"

#include <string>
#include <vector>

namespace mopr
{

struct Messaging
{
    const Client * client;
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

    Messaging( const Client * client );

    void
     debugPrint( );

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
     populateEditorLayout( CommandQueue & commandQueue, int pixelsW, int pixelsH );

    unsigned int
     populateCommandOptions( std::vector< std::string > & commandOptions,
                             unsigned int idNode,
                             unsigned int idSub );

    unsigned int
     applyCommandOption( unsigned int idNode, unsigned int idSub, unsigned int idOpt );

  private:
    std::string
     requestGetAndSelectUri( const char * get, const char * select );

    std::string
     locateEndpoint( const char * uriResource, const char * endpoint );

    std::string
     locateResourceWorkshop( const std::string & uriEndpointWorkshop );

    std::string
     locateResourceProject( const std::string & uriEndpointProject,
                            const AppEnvironment * appEnvironment );

    std::string
     manageProjectLock( const std::string & uriResourceProject, bool acquire );

    std::string
     locateResourceAsset( const std::string & uriEndpointAsset,
                          const AppEnvironment * appEnvironment );
};

}   // namespace mopr

#endif   // MOPR_MAIN_MESSAGING_H
