#include "editorProgram.h"

#include "glUtil.h"

#include <yoga/Yoga.h>

#include <stdio.h>

namespace mopr
{

bool
 EditorProgram::init( )
{
    // Generate program.
    this->pid = glCreateProgram( );

    // Vertex shader.
    {
        GLuint shdr = glCreateShader( GL_VERTEX_SHADER );

        const GLchar * src[] = { R"SHADER(

#version 150

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

#version 150

uniform vec3 layerColor;
out vec4 oColor;

void main()
{
    oColor = vec4( layerColor, 1. );
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

    // Get vertex attribute location.
    {
        this->pos2d = glGetAttribLocation( this->pid, "iPos2d" );

        if ( this->pos2d == -1 )
        {
            printf( "iPos2d is not a valid glsl program variable.\n" );
            return false;
        }
    }

    this->clr = glGetUniformLocation( this->pid, "layerColor" );

    // Initialize clear color.
    glClearColor( 0.f, 0.f, 0.f, 1.f );

    return true;
}

}   // namespace mopr
