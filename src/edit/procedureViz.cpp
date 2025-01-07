#include "procedureViz.h"

#include <iostream>

namespace mopr
{

CommandSettings::CommandSettings( ) : roundingFactor( 4.0f )
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

struct DrawBounds
{
    ImVec2 pMin;
    ImVec2 pMax;

    DrawBounds( CommandBase const * command, CommandContext const * ctxCmd )
    {
        pMin = { ctxCmd->offset.x + command->x, ctxCmd->offset.y + command->y };
        pMax = { pMin.x + command->w, pMin.y + command->h };
    }
};

CommandDrawRootContainer::CommandDrawRootContainer( ) : CommandBase( )
{
}

CommandDrawExprContainer::CommandDrawExprContainer( ) : CommandBase( )
{
}

CommandDrawLabel::CommandDrawLabel( CommandTheme bg, const char * text )
    : CommandBase( ), bg( bg ), text( text )
{
}

CommandDrawExprLabel::CommandDrawExprLabel( CommandTheme bg, const char * text )
    : CommandDrawLabel( bg, text )
{
}

CommandDrawAttrLabel::CommandDrawAttrLabel( CommandTheme bg, const char * text )
    : CommandDrawLabel( bg, text )
{
}

CommandDrawAttrInput::CommandDrawAttrInput( const char * text )
    : CommandBase( ), text( text )
{
}

void
 CommandQueue::debugPrint( ) const
{
    std::cout << "CommandQueue contents: "
              << "\npixelsW: " << pixelsW << "\npixelsH: " << pixelsH;

    for ( const auto & c : commands )
    {
        std::cout << "\nCommand Type: " << std::string( c->cmdType( ) );
        c->debugPrint( );
    }
    std::cout << std::endl;
}

void
 CommandBase::debugPrint( ) const
{
    std::cout << "\n- x: " << x << "\n- y: " << y << "\n- w: " << w << "\n- h: " << h
              << std::endl;
}

void
 CommandDrawLabel::debugPrint( ) const
{
    CommandBase::debugPrint( );
    std::cout << "- bg: " << bg << "\n- text: " << text << std::endl;
}

void
 CommandDrawAttrInput::debugPrint( ) const
{
    CommandBase::debugPrint( );
    std::cout << "- text: " << text << std::endl;
}

bool
 CommandBase::apply( ImDrawList * draw_list, CommandContext const * ctxCmd ) const
{
    return false;
}

bool
 CommandDrawRootContainer::apply( ImDrawList * draw_list,
                                  CommandContext const * ctxCmd ) const
{
    const float rounding = 3 * ctxCmd->settings->roundingFactor;
    DrawBounds b{ this, ctxCmd };
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
 CommandDrawExprContainer::apply( ImDrawList * draw_list,
                                  CommandContext const * ctxCmd ) const
{
    const float rounding = 2 * ctxCmd->settings->roundingFactor;
    DrawBounds b{ this, ctxCmd };
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
 CommandDrawExprLabel::apply( ImDrawList * draw_list,
                              CommandContext const * ctxCmd ) const
{
    const float rounding = 1.5 * ctxCmd->settings->roundingFactor;
    DrawBounds b{ this, ctxCmd };

    ImGui::SetCursorScreenPos( b.pMin );
    ImGui::PushID( this->idNode );
    ImGui::PushID( this->idSub );
    ImGui::InvisibleButton( "canvas",
                            ImVec2( this->w, this->h ),
                            ImGuiButtonFlags_MouseButtonLeft
                             | ImGuiButtonFlags_MouseButtonRight );
    ImGui::PopID( );
    ImGui::PopID( );

    float thickness = 1.0;
    ImU32 clrBg = ctxCmd->settings->colorTheme[ this->bg ];
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
    else if ( ctxCmd->idSelected == this->idNode && ctxCmd->idSubSelected == this->idSub )
    {
        thickness = 2.0;
        clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_SELECTED_INPUT_BG ];
        clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }

    draw_list->AddRectFilled( b.pMin, b.pMax, clrBg, rounding );

    if ( ctxCmd->idSelected == this->idNode && ctxCmd->idSubSelected == this->idSub )
        draw_list->AddRect( b.pMin, b.pMax, clrFg, rounding, 0, thickness );

    draw_list->AddText( ctxCmd->fontInfos[ FONT_ROLE_DEFAULT ].fontPtr,
                        ctxCmd->fontInfos[ FONT_ROLE_DEFAULT ].fontSize,
                        ImVec2{ b.pMin.x + 8, b.pMin.y + 4 },
                        clrTx,
                        this->text.c_str( ) );

    return retVal;
}

bool
 CommandDrawAttrLabel::apply( ImDrawList * draw_list,
                              CommandContext const * ctxCmd ) const
{
    const float rounding = 1 * ctxCmd->settings->roundingFactor;
    DrawBounds b{ this, ctxCmd };

    ImGui::SetCursorScreenPos( b.pMin );
    ImGui::PushID( this->idNode );
    ImGui::PushID( this->idSub );
    ImGui::InvisibleButton( "canvas",
                            ImVec2( this->w, this->h ),
                            ImGuiButtonFlags_MouseButtonLeft
                             | ImGuiButtonFlags_MouseButtonRight );
    ImGui::PopID( );
    ImGui::PopID( );

    float thickness = 1.0;
    ImU32 clrBg = ctxCmd->settings->colorTheme[ this->bg ];
    ImU32 clrFg = ctxCmd->settings->colorTheme[ this->bg ];
    ImU32 clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_LABEL_FG ];

    bool retVal =
     ImGui::IsItemHovered( ) && ImGui::IsMouseClicked( ImGuiMouseButton_Left );

    if ( ImGui::IsItemActive( ) )
    {
        clrBg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_INPUT_BG ];
        clrFg = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
        clrTx = ctxCmd->settings->colorTheme[ COMMAND_THEME_ATTR_ACTIVE_FG ];
    }
    else if ( ctxCmd->idSelected == this->idNode && ctxCmd->idSubSelected == this->idSub )
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
                        this->text.c_str( ) );

    return retVal;
}

bool
 CommandDrawAttrInput::apply( ImDrawList * draw_list,
                              CommandContext const * ctxCmd ) const
{
    const float rounding = 1 * ctxCmd->settings->roundingFactor;
    DrawBounds b{ this, ctxCmd };

    ImGui::SetCursorScreenPos( b.pMin );
    ImGui::PushID( this->idNode );
    ImGui::PushID( this->idSub );
    ImGui::InvisibleButton( "canvas",
                            ImVec2( this->w, this->h ),
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
    else if ( ctxCmd->idSelected == this->idNode && ctxCmd->idSubSelected == this->idSub )
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
                        this->text.c_str( ) );

    return retVal;
}

}   // namespace mopr
