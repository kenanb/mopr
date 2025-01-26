#include "guiConfig.h"

namespace mopr
{

GuiConfig::GuiConfig( ) : fnt( ), cmd( )
{
}

bool
 GuiConfig::Init( const AppEnvironment * appEnv, const AppConfig * appCfg )
{
    bool errors = false;

    ImGuiIO & io = ImGui::GetIO( );

    {
        int fontSize = appCfg->fontBaseSize;
        fnt[ FONT_ROLE_DEFAULT ].fontSize = fontSize;

        if ( appCfg->fontDefault.empty( ) )
        {
            fnt[ FONT_ROLE_DEFAULT ].fontPtr = NULL;
        }
        else
        {
            std::string relFontPath = "res/font/";
            relFontPath += appCfg->fontDefault;
            relFontPath += ".ttf";
            std::string absFontPath =
             appEnv->resolveAppRelativePath( relFontPath.c_str( ) );

            int mode = 0;
            if ( appEnv->statPath( absFontPath.c_str( ), &mode ) )
            {
                fnt[ FONT_ROLE_DEFAULT ].fontPtr =
                 io.Fonts->AddFontFromFileTTF( absFontPath.c_str( ), fontSize );
            }
            else
            {
                errors = true;
                fnt[ FONT_ROLE_DEFAULT ].fontPtr = NULL;
            }
        }
    }

    {
        int fontSize = appCfg->fontBaseSize + 4;
        fnt[ FONT_ROLE_HEADING ].fontSize = fontSize;

        if ( appCfg->fontHeading.empty( ) )
        {
            fnt[ FONT_ROLE_HEADING ].fontPtr = NULL;
        }
        else
        {
            std::string relFontPath = "res/font/";
            relFontPath += appCfg->fontHeading;
            relFontPath += ".ttf";
            std::string absFontPath =
             appEnv->resolveAppRelativePath( relFontPath.c_str( ) );

            int mode = 0;
            if ( appEnv->statPath( absFontPath.c_str( ), &mode ) )
            {
                fnt[ FONT_ROLE_HEADING ].fontPtr =
                 io.Fonts->AddFontFromFileTTF( absFontPath.c_str( ), fontSize );
            }
            else
            {
                errors = true;
                fnt[ FONT_ROLE_HEADING ].fontPtr = NULL;
            }
        }
    }

    return !errors;
}

CommandConfig::CommandConfig( ) : roundingFactor( 4.0f )
{
    colorTheme[ COMMAND_THEME_NONE ] = IM_COL32( 0, 0, 0, 255 );
    colorTheme[ COMMAND_THEME_ROOT_CONTAINER_FG ] = IM_COL32( 0, 0, 0, 125 );
    colorTheme[ COMMAND_THEME_ROOT_CONTAINER_BG ] = IM_COL32( 235, 220, 175, 125 );
    colorTheme[ COMMAND_THEME_EXPR_CONTAINER_FG ] = IM_COL32( 0, 0, 0, 125 );
    colorTheme[ COMMAND_THEME_EXPR_CONTAINER_BG ] = IM_COL32( 235, 235, 235, 150 );
    colorTheme[ COMMAND_THEME_EXPR_LABEL_FG ] = IM_COL32( 0, 0, 0, 255 );
    colorTheme[ COMMAND_THEME_EXPR_BG_0 ] = IM_COL32( 150, 190, 175, 75 );
    colorTheme[ COMMAND_THEME_EXPR_BG_1 ] = IM_COL32( 150, 225, 190, 75 );
    colorTheme[ COMMAND_THEME_EXPR_BG_2 ] = IM_COL32( 190, 150, 225, 75 );
    colorTheme[ COMMAND_THEME_EXPR_BG_3 ] = IM_COL32( 190, 225, 150, 75 );
    colorTheme[ COMMAND_THEME_EXPR_BG_4 ] = IM_COL32( 225, 150, 190, 75 );
    colorTheme[ COMMAND_THEME_EXPR_BG_5 ] = IM_COL32( 130, 160, 240, 75 );
    colorTheme[ COMMAND_THEME_EXPR_BG_6 ] = IM_COL32( 130, 240, 160, 75 );
    colorTheme[ COMMAND_THEME_EXPR_BG_7 ] = IM_COL32( 200, 200, 200, 75 );   // Reserved.
    colorTheme[ COMMAND_THEME_EXPR_BG_8 ] = IM_COL32( 200, 200, 200, 75 );   // Reserved.
    colorTheme[ COMMAND_THEME_EXPR_BG_9 ] = IM_COL32( 200, 200, 200, 75 );   // Reserved.
    colorTheme[ COMMAND_THEME_ATTR_LABEL_FG ] = IM_COL32( 0, 0, 0, 255 );
    colorTheme[ COMMAND_THEME_ATTR_INPUT_FG ] = IM_COL32( 150, 150, 150, 255 );
    colorTheme[ COMMAND_THEME_ATTR_INPUT_BG ] = IM_COL32( 255, 255, 255, 150 );
    colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ] = IM_COL32( 50, 50, 100, 255 );
    colorTheme[ COMMAND_THEME_ATTR_ACTIVE_INPUT_BG ] = IM_COL32( 255, 255, 255, 255 );
    colorTheme[ COMMAND_THEME_ATTR_SELECTED_INPUT_BG ] = IM_COL32( 75, 255, 75, 255 );
}

}   // namespace mopr
