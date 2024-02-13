#ifndef MOPR_WRAP_STD_BOX_STRING_H
#define MOPR_WRAP_STD_BOX_STRING_H

#include "wrap/_base/box/generic.h"

#include <string>

struct MOPR_API MoprString : public MoprGeneric
{
    std::string d;
};

#endif   // MOPR_WRAP_STD_BOX_STRING_H
