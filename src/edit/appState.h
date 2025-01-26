#ifndef MOPR_MAIN_APPSTATE_H
#define MOPR_MAIN_APPSTATE_H

#include <string>

#include "appConfig.h"
#include "appEnvironment.h"

namespace mopr
{

typedef enum NavigationState
{
    NAVIGATION_STATE_NONE = 0,
    NAVIGATION_STATE_ORBIT,
    NAVIGATION_STATE_PAN,
    NAVIGATION_STATE_ZOOM,
    // NAVIGATION_STATE_LOOK,
    // NAVIGATION_STATE_ROLL,
} NavigationState;

struct AppState
{
    // Defaulted to values from AppEnvironment.
    std::string projectPath;
    std::string assetPath;
    double frameFirst;
    double frameLast;

    // Defaulted to values from AppConfig.
    int screenW;
    int screenH;
    double viewRotate[ 2 ];
    double viewTranslate[ 3 ];

    // Defaulted to values from constructor.
    bool quit;
    bool showOverlays;
    NavigationState nav;
    std::string camera;
    unsigned int idSelected;
    unsigned int idSubSelected;
    double mx;
    double my;

    AppState( const AppEnvironment * appEnv, const AppConfig * appCfg );
};

}   // namespace mopr

#endif   // MOPR_MAIN_APPSTATE_H
