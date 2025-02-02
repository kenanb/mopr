#ifndef MOPR_MAIN_VIEWPORTNAVIGATION_H
#define MOPR_MAIN_VIEWPORTNAVIGATION_H

#include "appState.h"

#include "SDL3/SDL.h"

namespace mopr
{

extern void
 handleMouseButton( AppState * appState, const SDL_Event * e );

extern void
 handleMouseMotion( AppState * appState, const SDL_Event * e );

extern void
 handleKeyUp( AppState * appState, const SDL_Event * e );

}   // namespace mopr

#endif   // MOPR_MAIN_VIEWPORTNAVIGATION_H
