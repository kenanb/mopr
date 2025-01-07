#ifndef MOPR_MAIN_EDITOR_H
#define MOPR_MAIN_EDITOR_H

#include "procedureViz.h"

#include "imgui.h"

#include <string>
#include <vector>

namespace mopr
{

//
// Editor
//

struct Editor
{
    Editor( FontInfo const * const fontInfos ) : fontInfos( fontInfos )
    {
    }

    void
     draw( CommandQueue const & q,
           unsigned int * idSelected,
           unsigned int * idSubSelected );

    void
     drawOptions( const std::vector< std::string > & o, unsigned int * optSelected );

    FontInfo const * fontInfos;
};

}   // namespace mopr

#endif   // MOPR_MAIN_EDITOR_H
