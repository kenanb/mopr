#ifndef MOPR_MAIN_EDITOR_H
#define MOPR_MAIN_EDITOR_H

// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

#include <stddef.h>
#include <vector>

namespace mopr
{

//
// Editor
//

struct Layer
{
    int quadCount;
    GLuint vao;
    GLuint vbo;
    GLuint ibo;
    std::vector< GLfloat > vbuffer;
    std::vector< GLuint > ibuffer;
    float color[ 3 ];

    Layer( ) : quadCount( 0 ), vao( 0 ), vbo( 0 ), ibo( 0 )
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
     init( GLint pos2d );

    void
     draw( GLint pos2d, GLint clr ) const;

    void
     allocate( size_t quadCount )
    {
        this->quadCount = quadCount;

        this->vbuffer.resize( quadCount * 8 );

        size_t offset = 0;
        this->ibuffer.clear( );
        this->ibuffer.reserve( quadCount * 6 );
        for ( size_t i = 0; i < quadCount; i++ )
        {
            this->ibuffer.push_back( offset + 0 );
            this->ibuffer.push_back( offset + 1 );
            this->ibuffer.push_back( offset + 2 );
            this->ibuffer.push_back( offset + 2 );
            this->ibuffer.push_back( offset + 3 );
            this->ibuffer.push_back( offset + 0 );
            offset += 4;
        }
    }
};

struct Editor
{
    std::vector< Layer > layers;
    GLuint pid;
    GLint pos2d;
    GLint clr;

    Editor( ) : pid( 0 ), pos2d( -1 ), clr( -1 )
    {
    }

    void
     dummyTree( );

    bool
     init( );
};

}   // namespace mopr

#endif   // MOPR_MAIN_EDITOR_H
