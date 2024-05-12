#include "overlayLayer.h"

#include "glUtil.h"

#include "imgui.h"

#include <stdio.h>

namespace mopr
{

void
 OverlayLayer::init( const OverlayProgram & prog )
{
    {
        GLenum target = GL_ARRAY_BUFFER;
        size_t bSize = this->vbuffer.size( ) * sizeof( GLfloat );
        glGenBuffers( 1, &this->vbo );
        GL_CALL( glBindBuffer( target, this->vbo ) );
        GL_CALL( glBufferData( target, bSize, this->vbuffer.data( ), GL_STATIC_DRAW ) );
        GL_CALL( glVertexAttribPointer(
         prog.pos2d, 2, GL_FLOAT, GL_FALSE, 2 * sizeof( GLfloat ), NULL ) );
    }

    {
        GLenum target = GL_ELEMENT_ARRAY_BUFFER;
        size_t bSize = this->ibuffer.size( ) * sizeof( GLuint );
        glGenBuffers( 1, &this->ibo );
        GL_CALL( glBindBuffer( target, this->ibo ) );
        GL_CALL( glBufferData( target, bSize, this->ibuffer.data( ), GL_STATIC_DRAW ) );
    }
}

void
 OverlayLayer::fini( )
{
    GL_CALL( glDeleteBuffers( 1, &this->vbo ) );
    this->vbo = 0;

    GL_CALL( glDeleteBuffers( 1, &this->ibo ) );
    this->ibo = 0;
}

void
 OverlayLayer::draw( const OverlayProgram & prog ) const
{
    GL_CALL( glEnableVertexAttribArray( prog.pos2d ) );
    GL_CALL(
     glUniform3f( prog.clr, this->color[ 0 ], this->color[ 1 ], this->color[ 2 ] ) );
    GL_CALL( glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, this->ibo ) );
    GL_CALL(
     glDrawElements( GL_TRIANGLES, this->ibuffer.size( ), GL_UNSIGNED_INT, NULL ) );
    GL_CALL( glDisableVertexAttribArray( prog.pos2d ) );
}

void
 OverlayLayer::allocate( size_t quadCount )
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

void
 dummyOverlay( std::vector< OverlayLayer > & layers )
{
    // clang-format off
    layers.resize( 1 );
    GLfloat Lc0 = 0.75f;
    GLfloat Tc0 = 0.75f;
    GLfloat Wc0 = 0.1f;
    GLfloat Hc0 = 0.1f;
    layers[ 0 ].setColor( .25f, .25f, .25f );
    layers[ 0 ].allocate( 1 );
    layers[ 0 ].vbuffer = {
        Lc0      , Tc0,
        Lc0 + Wc0, Tc0,
        Lc0 + Wc0, Tc0 + Hc0,
        Lc0      , Tc0 + Hc0
    };
    // clang-format on
}

}   // namespace mopr
