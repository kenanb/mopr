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
    Editor( ) = default;

    void
     drawMenu( ) const;

    void
     drawMain( ) const;

    void
     drawParameters( ) const;

    void
     drawContents( ) const;

    void
     drawTree( CommandQueue const & commandQueue,
               std::vector< std::string > const & payloadOptions,
               unsigned int * idSelected,
               unsigned int * idSubSelected,
               unsigned int * optSelected ) const;
};

}   // namespace mopr

#endif   // MOPR_MAIN_EDITOR_H
