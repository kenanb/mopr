#ifndef MOPR_MAIN_MESSAGING_H
#define MOPR_MAIN_MESSAGING_H

#include "appEnvironment.h"

#include <string>

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

    Messaging( );

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

    void
     debugPrint( );
};

}   // namespace mopr

#endif   // MOPR_MAIN_MESSAGING_H
