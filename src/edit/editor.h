#ifndef MOPR_MAIN_EDITOR_H
#define MOPR_MAIN_EDITOR_H

#include "repr/command.h"

#include "imgui.h"

namespace mopr
{

typedef enum FontRole
{
    FONT_ROLE_DEFAULT = 0,
    FONT_ROLE_HEADING,
    FONT_ROLE_TERMINATOR
} FontRole;

struct FontInfo
{
    ImFont * fontPtr;
    float fontSize;
};

//
// Editor
//

struct Editor
{
    Editor( FontInfo const * const fontInfos ) : fontInfos( fontInfos )
    {
    }

    void
     draw( CommandQueue const * const q,
           unsigned int * idSelected,
           unsigned int * idSubSelected );

    void
     drawOptions( CommandOptions const * const o, unsigned int * optSelected );

    FontInfo const * fontInfos;
};

}   // namespace mopr

#endif   // MOPR_MAIN_EDITOR_H
