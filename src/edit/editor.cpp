#include "editor.h"

namespace mopr
{

struct CommandSettings
{
    float roundingFactor;
    ImU32 colorTheme[ COMMAND_THEME_TERMINATOR ];

    CommandSettings( )
    {
        roundingFactor = 4.0f;
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
        colorTheme[ COMMAND_THEME_EXPR_BG_7 ] =
         IM_COL32( 200, 200, 200, 75 );   // Reserved.
        colorTheme[ COMMAND_THEME_EXPR_BG_8 ] =
         IM_COL32( 200, 200, 200, 75 );   // Reserved.
        colorTheme[ COMMAND_THEME_EXPR_BG_9 ] =
         IM_COL32( 200, 200, 200, 75 );   // Reserved.
        colorTheme[ COMMAND_THEME_ATTR_LABEL_FG ] = IM_COL32( 0, 0, 0, 255 );
        colorTheme[ COMMAND_THEME_ATTR_INPUT_FG ] = IM_COL32( 150, 150, 150, 255 );
        colorTheme[ COMMAND_THEME_ATTR_INPUT_BG ] = IM_COL32( 255, 255, 255, 150 );
        colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ] = IM_COL32( 50, 50, 100, 255 );
        colorTheme[ COMMAND_THEME_ATTR_ACTIVE_INPUT_BG ] = IM_COL32( 255, 255, 255, 255 );
        colorTheme[ COMMAND_THEME_ATTR_SELECTED_INPUT_BG ] = IM_COL32( 75, 255, 75, 255 );
    }
};

struct CommandContext
{
    unsigned int idSelected;
    unsigned int idSubSelected;
    ImVec2 offset;
    FontInfo const * fontInfos;
    CommandSettings const * settings;
};

struct DrawBounds
{
    ImVec2 pMin;
    ImVec2 pMax;

    DrawBounds( CombinedCommand const * command, CommandContext const * ctxCmd )
    {
        const CommandBase * c = &command->base;
        pMin = { ctxCmd->offset.x + c->x, ctxCmd->offset.y + c->y };
        pMax = { pMin.x + c->w, pMin.y + c->h };
    }
};

typedef bool ( *fnDraw )( ImDrawList * draw_list,
                          CombinedCommand command,
                          CommandContext const * ctxCmd );

bool
 cmdBase( ImDrawList * draw_list, CombinedCommand command, CommandContext const * ctxCmd )
{
    return false;
}

bool
 cmdDrawRootContainer( ImDrawList * draw_list,
                       CombinedCommand command,
                       CommandContext const * ctxCmd )
{
    const float rounding = 3 * ctxCmd->settings->roundingFactor;
    // CommandDrawRootContainer * c = &command.drawRootContainer;
    DrawBounds b{ &command, ctxCmd };
    draw_list->AddRectFilled(
     b.pMin,
     b.pMax,
     ctxCmd->settings->colorTheme[ COMMAND_THEME_ROOT_CONTAINER_BG ],
     rounding );
    draw_list->AddRect( b.pMin,
                        b.pMax,
                        ctxCmd->settings->colorTheme[ COMMAND_THEME_ROOT_CONTAINER_FG ],
                        rounding );
    return false;
}

bool
 cmdDrawExprContainer( ImDrawList * draw_list,
                       CombinedCommand command,
                       CommandContext const * ctxCmd )
{
    const float rounding = 2 * ctxCmd->settings->roundingFactor;
    // CommandDrawExprContainer * c = &command.drawExprContainer;
    DrawBounds b{ &command, ctxCmd };
    draw_list->AddRectFilled(
     b.pMin,
     b.pMax,
     ctxCmd->settings->colorTheme[ COMMAND_THEME_EXPR_CONTAINER_BG ],
     rounding );
    draw_list->AddRect( b.pMin,
                        b.pMax,
                        ctxCmd->settings->colorTheme[ COMMAND_THEME_EXPR_CONTAINER_FG ],
                        rounding );
    return false;
}

bool
 cmdDrawExprLabel( ImDrawList * draw_list,
                   CombinedCommand command,
                   CommandContext const * ctxCmd )
{
    const float rounding = 1.5 * ctxCmd->settings->roundingFactor;
    CommandDrawExprLabel * c = &command.drawExprLabel;
    DrawBounds b{ &command, ctxCmd };

    ImGui::SetCursorScreenPos( b.pMin );
    ImGui::PushID( c->id );
    ImGui::PushID( c->idSub );
    ImGui::InvisibleButton( "canvas",
                            ImVec2( c->w, c->h ),
                            ImGuiButtonFlags_MouseButtonLeft
                             | ImGuiButtonFlags_MouseButtonRight );
    ImGui::PopID( );
    ImGui::PopID( );

    float thickness = 1.0;
    ImU32 clrBg = ctxCmd->settings->colorTheme[ c->bg ];
    ImU32 clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_INPUT_FG ];
    ImU32 clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_EXPR_LABEL_FG ];

    bool retVal =
     ImGui::IsItemHovered( ) && ImGui::IsMouseClicked( ImGuiMouseButton_Left );

    if ( ImGui::IsItemActive( ) )
    {
        clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_INPUT_BG ];
        clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }
    else if ( ctxCmd->idSelected == c->id && ctxCmd->idSubSelected == c->idSub )
    {
        thickness = 2.0;
        clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_SELECTED_INPUT_BG ];
        clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }

    draw_list->AddRectFilled( b.pMin, b.pMax, clrBg, rounding );

    if ( ctxCmd->idSelected == c->id && ctxCmd->idSubSelected == c->idSub )
        draw_list->AddRect( b.pMin, b.pMax, clrFg, rounding, 0, thickness );

    draw_list->AddText( ctxCmd->fontInfos[ FONT_ROLE_DEFAULT ].fontPtr,
                        ctxCmd->fontInfos[ FONT_ROLE_DEFAULT ].fontSize,
                        ImVec2{ b.pMin.x + 8, b.pMin.y + 4 },
                        clrTx,
                        c->text );

    return retVal;
}

bool
 cmdDrawAttrLabel( ImDrawList * draw_list,
                   CombinedCommand command,
                   CommandContext const * ctxCmd )
{
    const float rounding = 1 * ctxCmd->settings->roundingFactor;
    CommandDrawAttrLabel * c = &command.drawAttrLabel;
    DrawBounds b{ &command, ctxCmd };

    ImGui::SetCursorScreenPos( b.pMin );
    ImGui::PushID( c->id );
    ImGui::PushID( c->idSub );
    ImGui::InvisibleButton( "canvas",
                            ImVec2( c->w, c->h ),
                            ImGuiButtonFlags_MouseButtonLeft
                             | ImGuiButtonFlags_MouseButtonRight );
    ImGui::PopID( );
    ImGui::PopID( );

    float thickness = 1.0;
    ImU32 clrBg = ctxCmd->settings->colorTheme[ c->bg ];
    ImU32 clrFg = ctxCmd->settings->colorTheme[ c->bg ];
    ImU32 clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_LABEL_FG ];

    bool retVal =
     ImGui::IsItemHovered( ) && ImGui::IsMouseClicked( ImGuiMouseButton_Left );

    if ( ImGui::IsItemActive( ) )
    {
        clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_INPUT_BG ];
        clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }
    else if ( ctxCmd->idSelected == c->id && ctxCmd->idSubSelected == c->idSub )
    {
        thickness = 2.0;
        clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_SELECTED_INPUT_BG ];
        clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }

    draw_list->AddRectFilled( b.pMin, b.pMax, clrBg, rounding );

    draw_list->AddRect( b.pMin, b.pMax, clrFg, rounding, 0, thickness );

    draw_list->AddText( ctxCmd->fontInfos[ FONT_ROLE_DEFAULT ].fontPtr,
                        ctxCmd->fontInfos[ FONT_ROLE_DEFAULT ].fontSize,
                        ImVec2{ b.pMin.x + 8, b.pMin.y + 8 },
                        clrTx,
                        c->text );

    return retVal;
}

bool
 cmdDrawAttrInput( ImDrawList * draw_list,
                   CombinedCommand command,
                   CommandContext const * ctxCmd )
{
    const float rounding = 1 * ctxCmd->settings->roundingFactor;
    CommandDrawAttrInput * c = &command.drawAttrInput;
    DrawBounds b{ &command, ctxCmd };

    ImGui::SetCursorScreenPos( b.pMin );
    ImGui::PushID( c->id );
    ImGui::PushID( c->idSub );
    ImGui::InvisibleButton( "canvas",
                            ImVec2( c->w, c->h ),
                            ImGuiButtonFlags_MouseButtonLeft
                             | ImGuiButtonFlags_MouseButtonRight );
    ImGui::PopID( );
    ImGui::PopID( );

    float thickness = 1.0;
    ImU32 clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_INPUT_BG ];
    ImU32 clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_INPUT_FG ];
    ImU32 clrTx = IM_COL32_BLACK;

    bool retVal =
     ImGui::IsItemHovered( ) && ImGui::IsMouseClicked( ImGuiMouseButton_Left );

    if ( ImGui::IsItemActive( ) )
    {
        clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_INPUT_BG ];
        clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }
    else if ( ctxCmd->idSelected == c->id && ctxCmd->idSubSelected == c->idSub )
    {
        thickness = 2.0;
        clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_SELECTED_INPUT_BG ];
        clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }

    draw_list->AddRectFilled( b.pMin, b.pMax, clrBg, rounding );

    draw_list->AddRect( b.pMin, b.pMax, clrFg, rounding, 0, thickness );

    draw_list->AddText( ctxCmd->fontInfos[ FONT_ROLE_DEFAULT ].fontPtr,
                        ctxCmd->fontInfos[ FONT_ROLE_DEFAULT ].fontSize,
                        ImVec2{ b.pMin.x + 8, b.pMin.y + 8 },
                        clrTx,
                        c->text );

    return retVal;
}

const fnDraw COMMANDS[] = {
    &cmdBase,          &cmdDrawRootContainer, &cmdDrawExprContainer,
    &cmdDrawExprLabel, &cmdDrawAttrLabel,     &cmdDrawAttrInput,
};

void
 Editor::draw( CommandQueue const * const q,
               unsigned int * idSelected,
               unsigned int * idSubSelected )
{
    ImGuiViewport * viewport = ImGui::GetMainViewport( );
    int windowWidth = viewport->Size.x / 2 + 30;
    int windowHeight = viewport->Size.y;
    ImGui::SetNextWindowPos(
     ImVec2( viewport->Pos.x + viewport->Size.x - windowWidth,   // Right align.
             viewport->Pos.y ) );
    ImGui::SetNextWindowSize( ImVec2( windowWidth, windowHeight ) );
    ImGui::SetNextWindowViewport( viewport->ID );

    ImGuiWindowFlags windowFlags =
     ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_NoTitleBar
     | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoBackground
     | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoResize
     | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoBringToFrontOnFocus
     | ImGuiWindowFlags_NoNavFocus;

    if ( !ImGui::Begin( "Procedural Tree", nullptr, windowFlags ) )
    {
        ImGui::End( );
        return;
    }

    // ImGui::Text( "..." );

    ImDrawList * draw_list = ImGui::GetWindowDrawList( );

    // Get the current ImGui cursor position
    ImVec2 offset = ImGui::GetCursorScreenPos( );
    static const CommandSettings settings;
    CommandContext ctxCmd{
        *idSelected, *idSubSelected, offset, this->fontInfos, &settings
    };

    for ( int i = 0; i < q->nofCommands; i++ )
    {
        if ( COMMANDS[ q->commands[ i ].base.cType ](
              draw_list, q->commands[ i ], &ctxCmd ) )
        {
            const unsigned int id = q->commands[ i ].base.id;
            const unsigned int idSub = q->commands[ i ].base.idSub;
            if ( ctxCmd.idSelected == id && ctxCmd.idSubSelected == idSub )
            {
                ctxCmd.idSelected = 0;
                ctxCmd.idSubSelected = 0;
            }
            else
            {
                ctxCmd.idSelected = id;
                ctxCmd.idSubSelected = idSub;
            }
        }
    }

    *idSelected = ctxCmd.idSelected;
    *idSubSelected = ctxCmd.idSubSelected;

    // Advance the ImGui cursor to claim space. If the "reserved" height and width were
    // not fully used, we expect the values to have already been adjusted to "used" area.
    ImGui::SetCursorScreenPos( ImVec2( offset.x + q->pixelsW, offset.y + q->pixelsH ) );

    ImGui::End( );
}

bool
 drawOption( ImDrawList * draw_list,
             const char * name,
             unsigned int i,
             CommandContext const * ctxCmd,
             unsigned int optSelected )
{
    int width = 300;
    int height = 36;
    const float rounding = 1 * ctxCmd->settings->roundingFactor;
    ImVec2 pMin = { ctxCmd->offset.x, ctxCmd->offset.y + ( i - 1 ) * ( height + 4 ) };
    ImVec2 pMax = { pMin.x + width, pMin.y + height };

    ImGui::SetCursorScreenPos( pMin );
    ImGui::PushID( name );
    ImGui::InvisibleButton( "canvas",
                            ImVec2( width, height ),
                            ImGuiButtonFlags_MouseButtonLeft
                             | ImGuiButtonFlags_MouseButtonRight );
    ImGui::PopID( );

    float thickness = 1.0;
    ImU32 clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_INPUT_BG ];
    ImU32 clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_INPUT_FG ];
    ImU32 clrTx = IM_COL32_BLACK;

    bool retVal =
     ImGui::IsItemHovered( ) && ImGui::IsMouseClicked( ImGuiMouseButton_Left );

    if ( ImGui::IsItemActive( ) )
    {
        clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_INPUT_BG ];
        clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }
    else if ( i == optSelected )
    {
        thickness = 2.0;
        clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_SELECTED_INPUT_BG ];
        clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }

    draw_list->AddRectFilled( pMin, pMax, clrBg, rounding );

    draw_list->AddRect( pMin, pMax, clrFg, rounding, 0, thickness );

    draw_list->AddText( ctxCmd->fontInfos[ FONT_ROLE_DEFAULT ].fontPtr,
                        ctxCmd->fontInfos[ FONT_ROLE_DEFAULT ].fontSize,
                        ImVec2{ pMin.x + 8, pMin.y + 8 },
                        clrTx,
                        name );

    return retVal;
}

void
 Editor::drawOptions( CommandOptions const * const o, unsigned int * optSelected )
{
    ImGuiViewport * viewport = ImGui::GetMainViewport( );
    int windowWidth = viewport->Size.x / 2;
    int windowHeight = viewport->Size.y;
    ImGui::SetNextWindowPos( ImVec2( viewport->Pos.x, viewport->Pos.y ) );
    ImGui::SetNextWindowSize( ImVec2( windowWidth, windowHeight ) );
    ImGui::SetNextWindowViewport( viewport->ID );

    ImGuiWindowFlags windowFlags =
     ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_NoTitleBar
     | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoBackground
     | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoResize
     | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoBringToFrontOnFocus
     | ImGuiWindowFlags_NoNavFocus;

    if ( !ImGui::Begin( "Command Options", nullptr, windowFlags ) )
    {
        ImGui::End( );
        return;
    }

    // ImGui::Text( "..." );

    ImDrawList * draw_list = ImGui::GetWindowDrawList( );

    // Get the current ImGui cursor position
    ImVec2 offset = ImGui::GetCursorScreenPos( );
    static const CommandSettings settings;
    CommandContext ctxCmd{ 0, 0, offset, this->fontInfos, &settings };

    for ( int i = 0; i < o->nofOptions; i++ )
    {
        unsigned int idx = i + 1;
        if ( drawOption( draw_list, o->options[ i ], idx, &ctxCmd, *optSelected ) )
        {
            *optSelected = idx;
        }
    }

    // Advance the ImGui cursor to claim space. If the "reserved" height and width were
    // not fully used, we expect the values to have already been adjusted to "used" area.
    ImGui::SetCursorScreenPos(
     ImVec2( offset.x + 300, offset.y + ( o->nofOptions * 40 ) ) );

    ImGui::End( );
}

}   // namespace mopr
