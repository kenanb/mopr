#ifndef MOPR_MAIN_APPDELEGATE_H
#define MOPR_MAIN_APPDELEGATE_H

#include "appEnvironment.h"

#include "SDL.h"

namespace mopr
{

extern void
 appDelegate( SDL_Window * window, const AppEnvironment * appEnvironment );

}   // namespace mopr

#endif   // MOPR_MAIN_APPDELEGATE_H
