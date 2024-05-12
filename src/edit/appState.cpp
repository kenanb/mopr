#include "appState.h"

namespace mopr
{

void
 handleMouseMotion( AppState * appState, const SDL_Event * e )
{
    int x, y;
    SDL_GetMouseState( &x, &y );
    appState->viewRotate[ 0 ] += x - appState->mx;
    appState->viewRotate[ 1 ] += y - appState->my;
    appState->mx = x;
    appState->my = y;
}

void
 handleKeyUp( AppState * appState, const SDL_Event * e )
{
    SDL_KeyboardEvent eKeyboard = e->key;
    switch ( eKeyboard.keysym.sym )
    {
        case SDLK_ESCAPE:
            appState->quit = true;
            break;
        case SDLK_TAB:
            appState->showOverlays = !appState->showOverlays;
            break;
    }
}

}   // namespace mopr
