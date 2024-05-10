#ifndef MOPR_MAIN_EDITORLAYER_H
#define MOPR_MAIN_EDITORLAYER_H

#include "editorProgram.h"

// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

#include <stddef.h>
#include <vector>

namespace mopr
{

//
// Editor
//

struct EditorLayer
{
    int quadCount;
    GLuint vao;
    GLuint vbo;
    GLuint ibo;
    std::vector< GLfloat > vbuffer;
    std::vector< GLuint > ibuffer;
    float color[ 3 ];

    EditorLayer( ) : quadCount( 0 ), vao( 0 ), vbo( 0 ), ibo( 0 )
    {
        this->setColor( .0f, .0f, .0f );
    }

    void
     setColor( float r, float g, float b )
    {
        this->color[ 0 ] = r;
        this->color[ 1 ] = g;
        this->color[ 2 ] = b;
    }

    void
     init( const EditorProgram & prog );

    void
     draw( const EditorProgram & prog ) const;

    void
     allocate( size_t quadCount );
};

void
 dummyTree( std::vector< EditorLayer > & layers );

}   // namespace mopr

#endif   // MOPR_MAIN_EDITORLAYER_H
