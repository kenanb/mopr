#ifndef MOPR_MAIN_GLUTIL_H
#define MOPR_MAIN_GLUTIL_H

// Debugging:
//#define MOPR_GL_DEBUG
#ifdef MOPR_GL_DEBUG
#    include <stdio.h>
#    define GL_CALL( _CALL )                                                             \
        do                                                                               \
        {                                                                                \
            _CALL;                                                                       \
            GLenum err = glGetError( );                                                  \
            if ( err != 0 )                                                              \
                fprintf( stderr, "Call '%s' returned error: 0x%x\n", #_CALL, err );      \
        } while ( 0 )
#else
#    define GL_CALL( _CALL ) _CALL
#endif

namespace mopr
{

extern void
 printProgramLog( GLuint program );

extern void
 printShaderLog( GLuint shader );

extern bool
 compileShader( GLuint shdr, const GLchar ** src );

}   // namespace mopr

#endif   // MOPR_MAIN_GLUTIL_H
