// GL API providers (GLEW, GLApi) should be included before other GL headers.
#include "pxr/imaging/garch/glApi.h"

#include "glUtil.h"

#include "common.h"

#include <stdio.h>
#include <stdlib.h>

namespace mopr
{

void
 printProgramLog( GLuint program )
{
    if ( !glIsProgram( program ) )
    {
        printf( "Program log: %d is not a program.\n", program );
        return;
    }

    int infoLogLength = 0;
    int allocLength = 0;

    glGetProgramiv( program, GL_INFO_LOG_LENGTH, &allocLength );

    char * infoLog = static_cast< char * >( xmalloc( sizeof( char ) * allocLength ) );

    glGetProgramInfoLog( program, allocLength, &infoLogLength, infoLog );
    if ( infoLogLength > 0 ) printf( "%s\n", infoLog );

    free( infoLog );
}

void
 printShaderLog( GLuint shader )
{
    if ( !glIsShader( shader ) )
    {
        printf( "Program log: %d is not a shader.\n", shader );
        return;
    }

    int infoLogLength = 0;
    int allocLength = 0;

    glGetShaderiv( shader, GL_INFO_LOG_LENGTH, &allocLength );

    char * infoLog = static_cast< char * >( xmalloc( sizeof( char ) * allocLength ) );

    glGetShaderInfoLog( shader, allocLength, &infoLogLength, infoLog );
    if ( infoLogLength > 0 ) printf( "%s\n", infoLog );

    free( infoLog );
}

bool
 compileShader( GLuint shdr, const GLchar ** src )
{
    // Set vertex source
    glShaderSource( shdr, 1, src, NULL );

    // Compile vertex source
    glCompileShader( shdr );

    // Check vertex shader for errors
    GLint shdrCompiled = GL_FALSE;
    glGetShaderiv( shdr, GL_COMPILE_STATUS, &shdrCompiled );

    if ( shdrCompiled != GL_TRUE )
    {
        printf( "Unable to compile shader %d!\n", shdr );
        printShaderLog( shdr );
        return false;
    }

    return true;
}

}   // namespace mopr
