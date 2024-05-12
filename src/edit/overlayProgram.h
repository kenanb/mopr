#ifndef MOPR_MAIN_OVERLAYPROGRAM_H
#define MOPR_MAIN_OVERLAYPROGRAM_H

// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

namespace mopr
{

//
// OverlayProgram
//

struct OverlayProgram
{
    GLuint pid;
    GLint pos2d;
    GLint clr;

    OverlayProgram( ) : pid( 0 ), pos2d( -1 ), clr( -1 )
    {
    }

    bool
     init( );

    void
     fini( );
};

}   // namespace mopr

#endif   // MOPR_MAIN_OVERLAYPROGRAM_H
