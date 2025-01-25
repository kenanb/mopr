#ifndef MOPR_MAIN_APPSTATE_H
#define MOPR_MAIN_APPSTATE_H

#include <string>

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
    int screenW;
    int screenH;
    bool quit;
    bool showOverlays;
    NavigationState nav;
    unsigned int idSelected;
    unsigned int idSubSelected;
    double mx;
    double my;
    double viewRotate[ 2 ];
    double viewTranslate[ 3 ];

    AppState( int screenW, int screenH );
};

}   // namespace mopr

#endif   // MOPR_MAIN_APPSTATE_H
