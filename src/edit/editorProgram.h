#ifndef MOPR_MAIN_EDITORPROGRAM_H
#define MOPR_MAIN_EDITORPROGRAM_H

// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

namespace mopr
{

//
// EditorProgram
//

struct EditorProgram
{
    GLuint pid;
    GLint pos2d;
    GLint clr;

    EditorProgram( ) : pid( 0 ), pos2d( -1 ), clr( -1 )
    {
    }

    bool
     init( );

    void
     fini( );
};

}   // namespace mopr

#endif   // MOPR_MAIN_EDITORPROGRAM_H
