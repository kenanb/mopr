#ifndef MOPR_MAIN_APPSTATE_H
#define MOPR_MAIN_APPSTATE_H

#include "SDL.h"

namespace mopr
{

//
// AppState
//

typedef enum NavigationState {
    NAVIGATION_STATE_NONE = 0,
    NAVIGATION_STATE_ORBIT,
    NAVIGATION_STATE_PAN,
    NAVIGATION_STATE_ZOOM,
    // NAVIGATION_STATE_LOOK,
    // NAVIGATION_STATE_ROLL,
} NavigationState;

typedef struct AppState
{
    int screenW;
    int screenH;
    bool quit;
    bool showOverlays;
    NavigationState nav;
    double mx;
    double my;
    double viewRotate[ 2 ];
    double viewTranslate[ 3 ];
} AppState;

extern void
 handleMouseButton( AppState * appState, const SDL_Event * e );

extern void
 handleMouseMotion( AppState * appState, const SDL_Event * e );

extern void
 handleKeyUp( AppState * appState, const SDL_Event * e );

}   // namespace mopr

#endif   // MOPR_MAIN_APPSTATE_H
