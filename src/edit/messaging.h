#ifndef MOPR_MAIN_MESSAGING_H
#define MOPR_MAIN_MESSAGING_H

#include "client.h"
#include "procedureViz.h"

#include <string>
#include <unordered_map>
#include <vector>

namespace mopr
{

struct MessagingBase
{
    const Client * client;
    std::string base;
    std::string uuid;

    MessagingBase( const Client * client );

    virtual ~MessagingBase( ) = default;

    virtual std::string
     getBaseResourceSelector( ) const = 0;

    virtual void
     debugPrint( ) const = 0;

  protected:
    unsigned int
     requestGetAndSetBaseInfo( const char * get );

    std::string
     requestGetAndSelectUri( const char * get, const char * select ) const;

    std::string
     locateEndpoint( const char * uriResource, const char * endpoint ) const;
};

struct AssetMessaging : public MessagingBase
{
    std::string path;
    std::string uriEpStaging;
    std::string uriEpWorking;
    std::string uriResBound;

    AssetMessaging( const Client * client, const std::string & assetPath );

    virtual ~AssetMessaging( ) = default;

    virtual std::string
     getBaseResourceSelector( ) const override
    {
        std::string selection = "//assets/asset[@path='";
        selection += path;
        selection += "']";
        return selection;
    }

    virtual void
     debugPrint( ) const override;

    unsigned int
     initGenericEndpoints( const std::string & uriEndpointAsset );

    unsigned int
     bindStaging( );

    unsigned int
     initInteraction( ) const;

    unsigned int
     termInteraction( ) const;

    unsigned int
     exportToUsd( std::string & workshopRelPath ) const;

    unsigned int
     populateEditorLayout( CommandQueue & commandQueue, int pixelsW, int pixelsH ) const;

    unsigned int
     populateCommandOptions( std::vector< std::string > & commandOptions,
                             unsigned int idNode,
                             unsigned int idSub ) const;

    unsigned int
     applyCommandOption( unsigned int idNode,
                         unsigned int idSub,
                         unsigned int idOpt ) const;
};

struct ProjectMessaging : public MessagingBase
{
    std::string path;
    std::string uriEpPL;
    std::string uriEpA;
    std::string pLockState;
    std::unordered_map< std::string, AssetMessaging > assetPathToMessaging;

    ProjectMessaging( const Client * client, const std::string & projectPath );

    virtual ~ProjectMessaging( ) = default;

    virtual std::string
     getBaseResourceSelector( ) const override
    {
        std::string selection = "//projects/project[@path='";
        selection += path;
        selection += "']";
        return selection;
    }

    AssetMessaging &
     getOrCreateAssetMessaging( const std::string & assetPath );

    virtual void
     debugPrint( ) const override;

    unsigned int
     initGenericEndpoints( const std::string & uriEndpointProject );

    unsigned int
     initProjectEndpoints( );

    unsigned int
     acquireProject( );

    unsigned int
     releaseProject( );

  private:
    std::string
     manageProjectLock( const std::string & uriResourceProject, bool acquire ) const;
};

struct WorkshopMessaging : public MessagingBase
{
    std::string uriEpW;
    std::string uriEpP;
    std::unordered_map< std::string, ProjectMessaging > projectPathToMessaging;

    WorkshopMessaging( const Client * client );

    virtual ~WorkshopMessaging( ) = default;

    virtual std::string
     getBaseResourceSelector( ) const override
    {
        return "//workshop";
    }

    ProjectMessaging &
     getOrCreateProjectMessaging( const std::string & projectPath );

    virtual void
     debugPrint( ) const override;

    unsigned int
     initGenericEndpoints( );
};

}   // namespace mopr

#endif   // MOPR_MAIN_MESSAGING_H
