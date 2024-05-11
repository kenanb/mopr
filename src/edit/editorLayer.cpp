#include "editorLayer.h"

#include "glUtil.h"

#include <yoga/Yoga.h>

#include <stdio.h>

namespace mopr
{

void
 EditorLayer::init( const EditorProgram & prog )
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
 EditorLayer::fini( )
{
    GL_CALL( glDeleteBuffers( 1, &this->vbo ) );
    this->vbo = 0;

    GL_CALL( glDeleteBuffers( 1, &this->ibo ) );
    this->ibo = 0;
}

void
 EditorLayer::draw( const EditorProgram & prog ) const
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
 EditorLayer::allocate( size_t quadCount )
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
 dummyTree( std::vector< EditorLayer > & layers )
{
    // For disabling pixel rounding:
    // YGConfigRef config = YGConfigNew( );
    // YGConfigSetPointScaleFactor( config, 0.0f );

    const float pixelsW = 640;
    const float pixelsH = 480;
    YGNodeRef root = YGNodeNew( );
    YGNodeStyleSetFlexDirection( root, YGFlexDirectionColumn );
    YGNodeStyleSetWidth( root, pixelsW );
    YGNodeStyleSetHeight( root, pixelsH );
    YGNodeStyleSetPadding( root, YGEdgeAll, 10.0f );

    YGNodeRef c0 = YGNodeNew( );
    YGNodeStyleSetFlexGrow( c0, 1.0f );
    YGNodeStyleSetMargin( c0, YGEdgeAll, 10.0f );

    YGNodeRef c1 = YGNodeNew( );
    YGNodeStyleSetFlexGrow( c1, 1.0f );
    YGNodeStyleSetMargin( c1, YGEdgeAll, 10.0f );

    YGNodeStyleSetFlexDirection( c1, YGFlexDirectionRow );
    YGNodeRef c1c0 = YGNodeNew( );
    YGNodeStyleSetFlexGrow( c1c0, 1.0f );
    YGNodeStyleSetMargin( c1c0, YGEdgeAll, 10.0f );
    YGNodeRef c1c1 = YGNodeNew( );
    YGNodeStyleSetFlexGrow( c1c1, 1.0f );
    YGNodeStyleSetMargin( c1c1, YGEdgeAll, 10.0f );
    YGNodeRef c1c2 = YGNodeNew( );
    YGNodeStyleSetFlexGrow( c1c2, 1.0f );
    YGNodeStyleSetMargin( c1c2, YGEdgeAll, 10.0f );

    YGNodeInsertChild( root, c0, 0 );
    YGNodeInsertChild( root, c1, 1 );
    YGNodeInsertChild( c1, c1c0, 0 );
    YGNodeInsertChild( c1, c1c1, 1 );
    YGNodeInsertChild( c1, c1c2, 2 );

    YGNodeCalculateLayout( root, YGUndefined, YGUndefined, YGDirectionLTR );

    float Lr = YGNodeLayoutGetLeft( root ) / pixelsW;
    float Rr = YGNodeLayoutGetRight( root ) / pixelsW;
    float Tr = YGNodeLayoutGetTop( root ) / pixelsH;
    float Br = YGNodeLayoutGetBottom( root ) / pixelsH;
    float Wr = YGNodeLayoutGetWidth( root ) / pixelsW;
    float Hr = YGNodeLayoutGetHeight( root ) / pixelsH;

    float Lc0 = YGNodeLayoutGetLeft( c0 ) / pixelsW;
    float Rc0 = YGNodeLayoutGetRight( c0 ) / pixelsW;
    float Tc0 = YGNodeLayoutGetTop( c0 ) / pixelsH;
    float Bc0 = YGNodeLayoutGetBottom( c0 ) / pixelsH;
    float Wc0 = YGNodeLayoutGetWidth( c0 ) / pixelsW;
    float Hc0 = YGNodeLayoutGetHeight( c0 ) / pixelsH;

    float Lc1 = YGNodeLayoutGetLeft( c1 ) / pixelsW;
    float Rc1 = YGNodeLayoutGetRight( c1 ) / pixelsW;
    float Tc1 = YGNodeLayoutGetTop( c1 ) / pixelsH;
    float Bc1 = YGNodeLayoutGetBottom( c1 ) / pixelsH;
    float Wc1 = YGNodeLayoutGetWidth( c1 ) / pixelsW;
    float Hc1 = YGNodeLayoutGetHeight( c1 ) / pixelsH;

    float Lc1c0 = YGNodeLayoutGetLeft( c1c0 ) / pixelsW;
    // float Rc1c0 = YGNodeLayoutGetRight( c1c0 ) / pixelsW;
    float Tc1c0 = YGNodeLayoutGetTop( c1c0 ) / pixelsH;
    // float Bc1c0 = YGNodeLayoutGetBottom( c1c0 ) / pixelsH;
    float Wc1c0 = YGNodeLayoutGetWidth( c1c0 ) / pixelsW;
    float Hc1c0 = YGNodeLayoutGetHeight( c1c0 ) / pixelsH;

    float Lc1c1 = YGNodeLayoutGetLeft( c1c1 ) / pixelsW;
    // float Rc1c1 = YGNodeLayoutGetRight( c1c1 ) / pixelsW;
    float Tc1c1 = YGNodeLayoutGetTop( c1c1 ) / pixelsH;
    // float Bc1c1 = YGNodeLayoutGetBottom( c1c1 ) / pixelsH;
    float Wc1c1 = YGNodeLayoutGetWidth( c1c1 ) / pixelsW;
    float Hc1c1 = YGNodeLayoutGetHeight( c1c1 ) / pixelsH;

    float Lc1c2 = YGNodeLayoutGetLeft( c1c2 ) / pixelsW;
    // float Rc1c2 = YGNodeLayoutGetRight( c1c2 ) / pixelsW;
    float Tc1c2 = YGNodeLayoutGetTop( c1c2 ) / pixelsH;
    // float Bc1c2 = YGNodeLayoutGetBottom( c1c2 ) / pixelsH;
    float Wc1c2 = YGNodeLayoutGetWidth( c1c2 ) / pixelsW;
    float Hc1c2 = YGNodeLayoutGetHeight( c1c2 ) / pixelsH;

    YGNodeFreeRecursive( root );

    printf( "\nCoordinates:" );
    const char * formatString = "\n%s L: %f, R: %f, T: %f, B: %f, W: %f, H: %f";
    printf( formatString, "- r : ", Lr, Rr, Tr, Br, Wr, Hr );
    printf( formatString, "- c0: ", Lc0, Rc0, Tc0, Bc0, Wc0, Hc0 );
    printf( formatString, "- c1: ", Lc1, Rc1, Tc1, Bc1, Wc1, Hc1 );
    printf( "\n" );

    // clang-format off
    layers.resize( 2 );

    layers[ 0 ].setColor( .25f, .25f, .25f );
    layers[ 0 ].allocate( 2 );
    layers[ 0 ].vbuffer = {
        Lc0      , Tc0,
        Lc0 + Wc0, Tc0,
        Lc0 + Wc0, Tc0 + Hc0,
        Lc0      , Tc0 + Hc0,
        Lc1      , Tc1,
        Lc1 + Wc1, Tc1,
        Lc1 + Wc1, Tc1 + Hc1,
        Lc1      , Tc1 + Hc1
    };

    layers[ 1 ].setColor( .50f, .50f, .50f );
    layers[ 1 ].allocate( 3 );
    layers[ 1 ].vbuffer = {
        Lc1 + Lc1c0        , Tc1 + Tc1c0,
        Lc1 + Lc1c0 + Wc1c0, Tc1 + Tc1c0,
        Lc1 + Lc1c0 + Wc1c0, Tc1 + Tc1c0 + Hc1c0,
        Lc1 + Lc1c0        , Tc1 + Tc1c0 + Hc1c0,
        Lc1 + Lc1c1        , Tc1 + Tc1c1,
        Lc1 + Lc1c1 + Wc1c1, Tc1 + Tc1c1,
        Lc1 + Lc1c1 + Wc1c1, Tc1 + Tc1c1 + Hc1c1,
        Lc1 + Lc1c1        , Tc1 + Tc1c1 + Hc1c1,
        Lc1 + Lc1c2        , Tc1 + Tc1c2,
        Lc1 + Lc1c2 + Wc1c2, Tc1 + Tc1c2,
        Lc1 + Lc1c2 + Wc1c2, Tc1 + Tc1c2 + Hc1c2,
        Lc1 + Lc1c2        , Tc1 + Tc1c2 + Hc1c2
    };
    // clang-format on
}

}   // namespace mopr
