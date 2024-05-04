#ifndef MOPR_MAIN_APPCONFIG_H
#define MOPR_MAIN_APPCONFIG_H

#include <string>
#include <vector>

int
 main( int argc, char * argv[] );

namespace mopr
{

struct AppConfig
{
    std::string renderer;
    int screenW;
    int screenH;
    double complexity;
    bool enableFrameAll;
    bool enablePurposeGuide;
    bool enablePurposeProxy;
    bool enablePurposeRender;
    bool enableLightingPlaceholder;
    bool enableLightingScene;
    bool enableLightingCamera;
    bool enableSceneMaterials;
    std::vector< double > viewTranslate;
    std::vector< double > viewRotate;

    static const AppConfig &
     GetInstance( )
    {
        return Instance( );
    }

  private:
    friend int ::main( int argc, char * argv[] );

    // Mutable access and initialization is only accessible through the main.
    static AppConfig &
     Instance( )
    {
        static AppConfig instance;
        return instance;
    }

    bool
     Init( const std::string & appConfigAbsPath );

    AppConfig( );
    AppConfig( AppConfig const & ) = delete;
    void
     operator=( AppConfig const & ) = delete;
};

}   // namespace mopr

#endif   // MOPR_MAIN_APPCONFIG_H
