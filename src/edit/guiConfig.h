#ifndef MOPR_MAIN_GUICONFIG_H
#define MOPR_MAIN_GUICONFIG_H

#include "appConfig.h"
#include "appEnvironment.h"

#include "repr/command.h"

#include "imgui.h"

int
 main( int argc, char * argv[] );

namespace mopr
{

typedef enum FontRole
{
    FONT_ROLE_DEFAULT = 0,
    FONT_ROLE_HEADING,
    FONT_ROLE_TERMINATOR
} FontRole;

struct FontInfo
{
    ImFont * fontPtr;
    float fontSize;
};

struct CommandConfig
{
    float roundingFactor;
    ImU32 colorTheme[ COMMAND_THEME_TERMINATOR ];

    CommandConfig( );
};

struct GuiConfig
{
    FontInfo fnt[ FONT_ROLE_TERMINATOR ];
    CommandConfig cmd;

    static const GuiConfig &
     GetInstance( )
    {
        return Instance( );
    }

  private:
    friend int ::main( int argc, char * argv[] );

    // Mutable access and initialization is only accessible through the main.
    static GuiConfig &
     Instance( )
    {
        static GuiConfig instance;
        return instance;
    }

    bool
     Init( const AppEnvironment * appEnv, const AppConfig * appCfg );

    GuiConfig( );
    GuiConfig( GuiConfig const & ) = delete;
    void
     operator=( GuiConfig const & ) = delete;
};

}   // namespace mopr

#endif   // MOPR_MAIN_GUICONFIG_H
