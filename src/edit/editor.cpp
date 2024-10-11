#include "editor.h"

#include "imgui.h"

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
        colorTheme[ COMMAND_THEME_ROOT_CONTAINER_BG ] = IM_COL32( 235, 220, 175, 75 );
        colorTheme[ COMMAND_THEME_EXPR_CONTAINER_FG ] = IM_COL32( 0, 0, 0, 125 );
        colorTheme[ COMMAND_THEME_EXPR_CONTAINER_BG ] = IM_COL32( 235, 235, 235, 150 );
        colorTheme[ COMMAND_THEME_ATTR_INPUT_FG ] = IM_COL32( 150, 150, 150, 255 );
        colorTheme[ COMMAND_THEME_ATTR_INPUT_BG ] = IM_COL32( 255, 255, 255, 150 );
        colorTheme[ COMMAND_THEME_ATTR_LABEL_FG ] = IM_COL32( 0, 0, 0, 255 );
        colorTheme[ COMMAND_THEME_EXPR_LABEL_FG ] = IM_COL32( 0, 0, 0, 255 );
        colorTheme[ COMMAND_THEME_EXPR_BG_0 ] = IM_COL32( 150, 190, 175, 75 );
        colorTheme[ COMMAND_THEME_EXPR_BG_1 ] = IM_COL32( 150, 225, 190, 75 );
        colorTheme[ COMMAND_THEME_EXPR_BG_2 ] = IM_COL32( 190, 150, 225, 75 );
        colorTheme[ COMMAND_THEME_EXPR_BG_3 ] = IM_COL32( 190, 225, 150, 75 );
        colorTheme[ COMMAND_THEME_EXPR_BG_4 ] = IM_COL32( 225, 150, 190, 75 );
        colorTheme[ COMMAND_THEME_EXPR_BG_5 ] = IM_COL32( 130, 160, 240, 75 );
        colorTheme[ COMMAND_THEME_EXPR_BG_6 ] = IM_COL32( 130, 240, 160, 75 );
        // Reserved:
        colorTheme[ COMMAND_THEME_EXPR_BG_7 ] = IM_COL32( 200, 200, 200, 75 );
        colorTheme[ COMMAND_THEME_EXPR_BG_8 ] = IM_COL32( 200, 200, 200, 75 );
        colorTheme[ COMMAND_THEME_EXPR_BG_9 ] = IM_COL32( 200, 200, 200, 75 );
    }
};

struct CommandContext
{
    ImVec2 offset;
    CommandOptions const * options;
};

typedef void ( *fnDraw )( ImDrawList * draw_list,
                          CombinedCommand command,
                          CommandContext const * ctxCmd );

void
 cmdBase( ImDrawList * draw_list, CombinedCommand command, CommandContext const * ctxCmd )
{
}

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

void
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
}

void
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
}

void
 cmdDrawExprLabel( ImDrawList * draw_list,
                   CombinedCommand command,
                   CommandContext const * ctxCmd )
{
    const float rounding = 1.5 * ctxCmd->options->roundingFactor;
    CommandDrawExprLabel * c = &command.drawExprLabel;
    DrawBounds b{ &command, ctxCmd };
    ImVec2 p_txt = { b.pMin.x + 8, b.pMin.y + 8 };
    draw_list->AddRectFilled(
     b.pMin, b.pMax, ctxCmd->options->colorTheme[ c->bg ], rounding );
    draw_list->AddText(
     p_txt, ctxCmd->options->colorTheme[ COMMAND_THEME_EXPR_LABEL_FG ], c->text );
}

void
 cmdDrawAttrLabel( ImDrawList * draw_list,
                   CombinedCommand command,
                   CommandContext const * ctxCmd )
{
    const float rounding = 1 * ctxCmd->options->roundingFactor;
    CommandDrawAttrLabel * c = &command.drawAttrLabel;
    DrawBounds b{ &command, ctxCmd };
    ImVec2 p_txt = { b.pMin.x + 8, b.pMin.y + 8 };
    draw_list->AddRectFilled(
     b.pMin, b.pMax, ctxCmd->options->colorTheme[ c->bg ], rounding );
    draw_list->AddRect( b.pMin, b.pMax, ctxCmd->options->colorTheme[ c->bg ], rounding );
    draw_list->AddText(
     p_txt, ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_LABEL_FG ], c->text );
}

void
 cmdDrawAttrInput( ImDrawList * draw_list,
                   CombinedCommand command,
                   CommandContext const * ctxCmd )
{
    const float rounding = 1 * ctxCmd->options->roundingFactor;
    CommandDrawAttrInput * c = &command.drawAttrInput;
    DrawBounds b{ &command, ctxCmd };
    ImVec2 p_txt = { b.pMin.x + 8, b.pMin.y + 8 };
    draw_list->AddRectFilled( b.pMin,
                              b.pMax,
                              ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_INPUT_BG ],
                              rounding );
    draw_list->AddRect( b.pMin,
                        b.pMax,
                        ctxCmd->options->colorTheme[ COMMAND_THEME_ATTR_INPUT_FG ],
                        rounding );
    draw_list->AddText( p_txt, IM_COL32_BLACK, c->text );
}

const fnDraw COMMANDS[] = {
    &cmdBase,          &cmdDrawRootContainer, &cmdDrawExprContainer,
    &cmdDrawExprLabel, &cmdDrawAttrLabel,     &cmdDrawAttrInput,
};

void
 Editor::draw( CommandQueue const * const q )
{
    ImGuiViewport * viewport = ImGui::GetMainViewport( );
    ImGui::SetNextWindowPos( ImVec2( viewport->Pos.x + 10, viewport->Pos.y + 10 ) );
    ImGui::SetNextWindowSize(
     ImVec2( viewport->Size.x / 2 + 30, viewport->Size.y - 20 ) );
    ImGui::SetNextWindowViewport( viewport->ID );

    ImGuiWindowFlags windowFlags =
     ImGuiWindowFlags_NoDocking
     // | ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoBackground
     | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoResize
     | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoBringToFrontOnFocus
     | ImGuiWindowFlags_NoNavFocus;

    if ( !ImGui::Begin( "Mopr Editor", nullptr, windowFlags ) )
    {
        ImGui::End( );
        return;
    }

    // ImGui::Text( "..." );

    ImDrawList * draw_list = ImGui::GetWindowDrawList( );

    // Get the current ImGui cursor position
    ImVec2 offset = ImGui::GetCursorScreenPos( );

    static const CommandOptions cmdOpt;
    const CommandContext ctxCmd{ offset, &cmdOpt };

    for ( int i = 0; i < q->nofCommands; i++ )
    {
        COMMANDS[ q->commands[ i ].base.cType ]( draw_list, q->commands[ i ], &ctxCmd );
    }

    // Advance the ImGui cursor to claim space. If the "reserved" height and width were
    // not fully used, we expect the values to have already been adjusted to "used" area.
    ImGui::Dummy( ImVec2( q->pixelsW, q->pixelsH ) );

    for ( int i = 0; i < 10; i++ ) ImGui::Text( "..." );

    ImGui::End( );
}

}   // namespace mopr
