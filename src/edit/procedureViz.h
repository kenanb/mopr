#ifndef MOPR_MAIN_PROCEDUREVIZ_H
#define MOPR_MAIN_PROCEDUREVIZ_H

#include "repr/command.h"

#include "imgui.h"

#include <memory>
#include <string>
#include <vector>

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

struct CommandSettings
{
    float roundingFactor;
    ImU32 colorTheme[ COMMAND_THEME_TERMINATOR ];

    CommandSettings( );
};

struct CommandContext
{
    unsigned int idSelected;
    unsigned int idSubSelected;
    ImVec2 offset;
    FontInfo const * fontInfos;
    CommandSettings const * settings;
};

struct CommandBase
{
    unsigned int idNode;
    unsigned int idSub;
    float x, y, w, h;

    CommandBase( ) = default;
    virtual ~CommandBase( ) = default;

    virtual void
     debugPrint( ) const;

    virtual bool
     apply( ImDrawList * draw_list, CommandContext const * ctxCmd ) const;

    virtual const char *
     cmdType( ) const
    {
        return "Base";
    }
};

struct CommandDrawRootContainer : CommandBase
{
    CommandDrawRootContainer( );

    virtual bool
     apply( ImDrawList * draw_list, CommandContext const * ctxCmd ) const override;

    virtual const char *
     cmdType( ) const override
    {
        return "DrawRootContainer";
    }
};

struct CommandDrawExprContainer : CommandBase
{
    CommandDrawExprContainer( );

    virtual bool
     apply( ImDrawList * draw_list, CommandContext const * ctxCmd ) const override;

    virtual const char *
     cmdType( ) const override
    {
        return "DrawExprContainer";
    }
};

struct CommandDrawLabel : CommandBase
{
    CommandTheme bg;
    std::string text;

    CommandDrawLabel( CommandTheme bg, const char * text );

    virtual void
     debugPrint( ) const override;
};

struct CommandDrawExprLabel : CommandDrawLabel
{
    CommandDrawExprLabel( CommandTheme bg, const char * text );

    virtual bool
     apply( ImDrawList * draw_list, CommandContext const * ctxCmd ) const override;

    virtual const char *
     cmdType( ) const override
    {
        return "DrawExprLabel";
    }
};

struct CommandDrawAttrLabel : CommandDrawLabel
{
    CommandDrawAttrLabel( CommandTheme bg, const char * text );

    virtual bool
     apply( ImDrawList * draw_list, CommandContext const * ctxCmd ) const override;

    virtual const char *
     cmdType( ) const override
    {
        return "DrawAttrLabel";
    }
};

struct CommandDrawAttrInput : CommandBase
{
    std::string text;

    CommandDrawAttrInput( const char * text );

    virtual void
     debugPrint( ) const override;

    virtual bool
     apply( ImDrawList * draw_list, CommandContext const * ctxCmd ) const override;

    virtual const char *
     cmdType( ) const override
    {
        return "DrawAttrInput";
    }
};

struct CommandQueue
{
    float pixelsW = 0.f;
    float pixelsH = 0.f;
    std::vector< std::shared_ptr< CommandBase > > commands;

    void
     clear( )
    {
        pixelsW = 0.f;
        pixelsH = 0.f;
        commands.clear( );
    }

    void
     debugPrint( ) const;
};

}   // namespace mopr

#endif   // MOPR_MAIN_PROCEDUREVIZ_H
