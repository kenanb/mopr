#include "editor.h"

namespace mopr
{

struct CommandOptions
{
    float roundingFactor;
    ImU32 colorTheme[ COMMAND_THEME_TERMINATOR ];

    CommandOptions( )
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
    CommandOptions const * options;
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
    const float rounding = 3 * ctxCmd->options->roundingFactor;
    // CommandDrawRootContainer * c = &command.drawRootContainer;
    DrawBounds b{ &command, ctxCmd };
    draw_list->AddRectFilled(
     b.pMin,
     b.pMax,
     ctxCmd->options->colorTheme[ COMMAND_THEME_ROOT_CONTAINER_BG ],
     rounding );
    draw_list->AddRect( b.pMin,
                        b.pMax,
                        ctxCmd->options->colorTheme[ COMMAND_THEME_ROOT_CONTAINER_FG ],
                        rounding );
    return false;
}

bool
 cmdDrawExprContainer( ImDrawList * draw_list,
                       CombinedCommand command,
                       CommandContext const * ctxCmd )
{
    const float rounding = 2 * ctxCmd->options->roundingFactor;
    // CommandDrawExprContainer * c = &command.drawExprContainer;
    DrawBounds b{ &command, ctxCmd };
    draw_list->AddRectFilled(
     b.pMin,
     b.pMax,
     ctxCmd->options->colorTheme[ COMMAND_THEME_EXPR_CONTAINER_BG ],
     rounding );
    draw_list->AddRect( b.pMin,
                        b.pMax,
                        ctxCmd->options->colorTheme[ COMMAND_THEME_EXPR_CONTAINER_FG ],
                        rounding );
    return false;
}

bool
 cmdDrawExprLabel( ImDrawList * draw_list,
                   CombinedCommand command,
                   CommandContext const * ctxCmd )
{
    const float rounding = 1.5 * ctxCmd->options->roundingFactor;
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
    ImU32 clrBg = ctxCmd->options->colorTheme[ c->bg ];
    ImU32 clrFg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_INPUT_FG ];
    ImU32 clrTx = ctxCmd->options->colorTheme[ COMMAND_THEME_EXPR_LABEL_FG ];

    bool retVal =
     ImGui::IsItemHovered( ) && ImGui::IsMouseClicked( ImGuiMouseButton_Left );

    if ( ImGui::IsItemActive( ) )
    {
        clrBg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_INPUT_BG ];
        clrFg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }
    else if ( ctxCmd->idSelected == c->id && ctxCmd->idSubSelected == c->idSub )
    {
        thickness = 2.0;
        clrBg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_SELECTED_INPUT_BG ];
        clrFg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
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
    const float rounding = 1 * ctxCmd->options->roundingFactor;
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
    ImU32 clrBg = ctxCmd->options->colorTheme[ c->bg ];
    ImU32 clrFg = ctxCmd->options->colorTheme[ c->bg ];
    ImU32 clrTx = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_LABEL_FG ];

    bool retVal =
     ImGui::IsItemHovered( ) && ImGui::IsMouseClicked( ImGuiMouseButton_Left );

    if ( ImGui::IsItemActive( ) )
    {
        clrBg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_INPUT_BG ];
        clrFg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }
    else if ( ctxCmd->idSelected == c->id && ctxCmd->idSubSelected == c->idSub )
    {
        thickness = 2.0;
        clrBg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_SELECTED_INPUT_BG ];
        clrFg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
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
    const float rounding = 1 * ctxCmd->options->roundingFactor;
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
    ImU32 clrBg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_INPUT_BG ];
    ImU32 clrFg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_INPUT_FG ];
    ImU32 clrTx = IM_COL32_BLACK;

    bool retVal =
     ImGui::IsItemHovered( ) && ImGui::IsMouseClicked( ImGuiMouseButton_Left );

    if ( ImGui::IsItemActive( ) )
    {
        clrBg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_INPUT_BG ];
        clrFg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }
    else if ( ctxCmd->idSelected == c->id && ctxCmd->idSubSelected == c->idSub )
    {
        thickness = 2.0;
        clrBg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_SELECTED_INPUT_BG ];
        clrFg = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
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
    static const CommandOptions cmdOpt;
    CommandContext ctxCmd{
        *idSelected, *idSubSelected, offset, this->fontInfos, &cmdOpt
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

}   // namespace mopr
