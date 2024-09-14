#ifndef MOPR_MAIN_EDITOR_H
#define MOPR_MAIN_EDITOR_H

#include "core/command.h"

namespace mopr
{

//
// Editor
//

struct Editor
{
    Editor( )
    {
    }

    void
     draw( CommandQueue const * const q );
};

}   // namespace mopr

#endif   // MOPR_MAIN_EDITOR_H
