#ifndef MOPR_ROUTINE_H
#define MOPR_ROUTINE_H

#include "base/api.h"

#include <string>

MOPR_API std::string
 generateUsdaString( const std::string & src, bool callEnabled = false );

MOPR_API std::string
 exportToUsdaString( const std::string & src );

MOPR_API void
 registerPlugins( );

#endif
