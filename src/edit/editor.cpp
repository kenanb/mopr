#include "editor.h"

#include "glUtil.h"

#include <yoga/Yoga.h>

#include <stdio.h>

namespace mopr
{

bool
 Editor::init( )
{
    // Generate program.
    this->pid = glCreateProgram( );

    // Vertex shader.
    {
        GLuint shdr = glCreateShader( GL_VERTEX_SHADER );

        const GLchar * src[] = { R"SHADER(

#version 150 core
in vec2 iPos2d;
void main()
{
    gl_Position = vec4( iPos2d.x, iPos2d.y, 0, 1 );
}

)SHADER" };

        if ( !compileShader( shdr, src ) ) return false;

        glAttachShader( this->pid, shdr );
    }

    // Fragment shader.
    {
        GLuint shdr = glCreateShader( GL_FRAGMENT_SHADER );

        const GLchar * src[] = { R"SHADER(

#version 150 core
out vec4 oColor;
void main()
{
    oColor = vec4( .5, .5, .5, 1. );
}

)SHADER" };

        if ( !compileShader( shdr, src ) ) return false;

        glAttachShader( this->pid, shdr );
    }

    glBindFragDataLocation( this->pid, 0, "oColor" );

    // Link program.
    {
        glLinkProgram( this->pid );

        GLint programSuccess = GL_TRUE;
        glGetProgramiv( this->pid, GL_LINK_STATUS, &programSuccess );

        if ( programSuccess != GL_TRUE )
        {
            printf( "Error linking program %d.\n", this->pid );
            printProgramLog( this->pid );
            return false;
        }
    }

    glGenVertexArrays( 1, &this->vao );
    glBindVertexArray( this->vao );

    // Get vertex attribute location.
    {
        this->pos2d = glGetAttribLocation( this->pid, "iPos2d" );

        if ( this->pos2d == -1 )
        {
            printf( "iPos2d is not a valid glsl program variable.\n" );
            return false;
        }
    }

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

    YGNodeInsertChild( root, c0, 0.0f );
    YGNodeInsertChild( root, c1, 1.0f );

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

    YGNodeFreeRecursive( root );

    printf( "\nCoordinates:" );
    const char * formatString = "\n%s L: %f, R: %f, T: %f, B: %f, W: %f, H: %f";
    printf( formatString, "- r : ", Lr, Rr, Tr, Br, Wr, Hr );
    printf( formatString, "- c0: ", Lc0, Rc0, Tc0, Bc0, Wc0, Hc0 );
    printf( formatString, "- c1: ", Lc1, Rc1, Tc1, Bc1, Wc1, Hc1 );
    printf( "\n" );

    // Initialize clear color.
    glClearColor( 0.f, 0.f, 0.f, 1.f );

    // clang-format off
    GLfloat vertexData[] = {
        Lc0      , Tc0,
        Lc0 + Wc0, Tc0,
        Lc0 + Wc0, Tc0 + Hc0,
        Lc0      , Tc0 + Hc0,
        Lc1      , Tc1,
        Lc1 + Wc1, Tc1,
        Lc1 + Wc1, Tc1 + Hc1,
        Lc1      , Tc1 + Hc1
    };
    // clang-format on

    glGenBuffers( 1, &this->vbo );
    glBindBuffer( GL_ARRAY_BUFFER, this->vbo );
    glBufferData( GL_ARRAY_BUFFER, sizeof( vertexData ), vertexData, GL_STATIC_DRAW );
    glVertexAttribPointer(
     this->pos2d, 2, GL_FLOAT, GL_FALSE, 2 * sizeof( GLfloat ), NULL );
    this->vbs = sizeof( vertexData ) / sizeof( vertexData[ 0 ] );

    GLuint indexData[] = { 0, 1, 2, 2, 3, 0, 4, 5, 6, 6, 7, 4 };

    glGenBuffers( 1, &this->ibo );
    glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, this->ibo );
    glBufferData(
     GL_ELEMENT_ARRAY_BUFFER, sizeof( indexData ), indexData, GL_STATIC_DRAW );
    this->ibs = sizeof( indexData ) / sizeof( indexData[ 0 ] );

    return true;
}

}   // namespace mopr
