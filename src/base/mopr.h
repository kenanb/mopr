#ifndef MOPR_H
#define MOPR_H

#define MOPR_MAJOR_VERSION 0
#define MOPR_MINOR_VERSION 0
#define MOPR_PATCH_VERSION 1

#define MOPR_VERSION MOPR_MAJOR_VERSION "." MOPR_MINOR_VERSION "." MOPR_PATCH_VERSION

#define MOPR_INTERNAL_NS_X_( MAJ, MIN ) moprInt_v##MAJ##_##MIN
#define MOPR_INTERNAL_NS_( MAJ, MIN ) MOPR_INTERNAL_NS_X_( MAJ, MIN )

#define MOPR_NS mopr
#define MOPR_INTERNAL_NS MOPR_INTERNAL_NS_( MOPR_MAJOR_VERSION, MOPR_MINOR_VERSION )
#define MOPR_NS_GLOBAL ::MOPR_NS

namespace MOPR_INTERNAL_NS
{
}

namespace MOPR_NS
{
using namespace MOPR_INTERNAL_NS;
}

#define MOPR_NAMESPACE_OPEN_SCOPE                                                        \
    namespace MOPR_INTERNAL_NS                                                           \
    {
#define MOPR_NAMESPACE_CLOSE_SCOPE }
#define MOPR_NAMESPACE_USING_DIRECTIVE using namespace MOPR_NS;

#endif   // MOPR_H
