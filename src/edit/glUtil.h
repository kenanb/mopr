#ifndef MOPR_MAIN_GLUTIL_H
#define MOPR_MAIN_GLUTIL_H

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
