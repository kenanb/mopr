#ifndef MOPR_MAIN_APPSTATE_H
#define MOPR_MAIN_APPSTATE_H

#include "SDL.h"

namespace mopr
{

//
// AppState
//

typedef struct AppState
{
    int screenW;
    int screenH;
    bool quit;
    bool showOverlays;
    double mx;
    double my;
    double viewRotate[ 2 ];
    double viewTranslate[ 3 ];
} AppState;

extern void
 handleMouseMotion( AppState * appState, const SDL_Event * e );

extern void
 handleKeyUp( AppState * appState, const SDL_Event * e );

}   // namespace mopr

#endif   // MOPR_MAIN_APPSTATE_H
