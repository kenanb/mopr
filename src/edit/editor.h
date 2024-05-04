#ifndef MOPR_MAIN_EDITOR_H
#define MOPR_MAIN_EDITOR_H

// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

namespace mopr
{

//
// Editor
//

struct Editor
{
    GLuint pid;
    GLuint vao;
    GLint pos2d;
    GLuint vbo;
    GLuint ibo;
    GLsizei vbs;
    GLsizei ibs;

    bool
     init( );
};

}   // namespace mopr

#endif   // MOPR_MAIN_EDITOR_H
