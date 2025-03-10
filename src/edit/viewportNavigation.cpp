#include "viewportNavigation.h"

namespace mopr
{

void
 handleMouseButton( AppState * appState, const SDL_Event * e )
{
    switch ( e->button.button )
    {
        case SDL_BUTTON_LEFT:
            // TODO
            break;
        case SDL_BUTTON_MIDDLE:
            if ( e->button.down )
            {
                switch ( SDL_GetModState( ) )
                {
                    case SDL_KMOD_NONE:
                        appState->nav = NAVIGATION_STATE_ORBIT;
                        break;
                    case SDL_KMOD_LSHIFT:
                    case SDL_KMOD_RSHIFT:
                        appState->nav = NAVIGATION_STATE_PAN;
                        break;
                    case SDL_KMOD_LCTRL:
                    case SDL_KMOD_RCTRL:
                        appState->nav = NAVIGATION_STATE_ZOOM;
                        break;
                    default:
                        break;
                }
            }
            else
            {
                appState->nav = NAVIGATION_STATE_NONE;
            }
            break;
        case SDL_BUTTON_RIGHT:
            // TODO
            break;
    }
}

void
 handleMouseMotion( AppState * appState, const SDL_Event * e )
{
    float x, y;
    SDL_GetMouseState( &x, &y );
    double dx = x - appState->mx;
    double dy = y - appState->my;

    switch ( appState->nav )
    {
        case NAVIGATION_STATE_NONE:
            break;

        case NAVIGATION_STATE_ORBIT:
            appState->viewRotate[ 0 ] += dx;
            appState->viewRotate[ 1 ] += dy;
            break;

        case NAVIGATION_STATE_PAN:
            // TODO : FIX PAN
            appState->viewTranslate[ 0 ] += dx;
            appState->viewTranslate[ 1 ] += dy;
            break;

        case NAVIGATION_STATE_ZOOM:
            appState->viewTranslate[ 2 ] += dy;
            break;
    }

    appState->mx = x;
    appState->my = y;
}

void
 handleKeyUp( AppState * appState, const SDL_Event * e )
{
    SDL_KeyboardEvent eKeyboard = e->key;
    switch ( eKeyboard.key )
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
