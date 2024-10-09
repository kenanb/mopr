#include "editor.h"

#include "imgui.h"

namespace mopr
{

typedef struct CommandContext
{
    ImVec2 offset;
    ImDrawFlags flags;
} CommandContext;

typedef void ( *fnDraw )( ImDrawList * draw_list, CombinedCommand command, CommandContext * ctxCmd );

void
 cmdNone( ImDrawList * draw_list, CombinedCommand command, CommandContext * ctxCmd )
{
}

void
 cmdDrawRect( ImDrawList * draw_list, CombinedCommand command, CommandContext * ctxCmd )
{
    CommandDrawRect * c = &command.drawRect;
    ImVec2 p_min = { ctxCmd->offset.x + c->x, ctxCmd->offset.y + c->y };
    ImVec2 p_max = { p_min.x + c->w, p_min.y + c->h };
    ImVec2 p_txt = { p_min.x + 5, p_min.y + 5 };
    ImU32 col = IM_COL32( c->col[ 0 ], c->col[ 1 ], c->col[ 2 ], c->col[ 3 ] );
    draw_list->AddRectFilled( p_min, p_max, col, c->rounding, ctxCmd->flags );
    draw_list->AddText(p_txt, IM_COL32_BLACK, c->text);
}

void
 cmdDrawCircle( ImDrawList * draw_list, CombinedCommand command, CommandContext * ctxCmd )
{
    CommandDrawCircle * c = &command.drawCircle;
    ImVec2 p_min = { ctxCmd->offset.x + c->x, ctxCmd->offset.y + c->y };
    ImVec2 p_max = { p_min.x + c->w, p_min.y + c->h };
    ImVec2 p_txt = { p_min.x + 5, p_min.y + 5 };
    ImU32 col = IM_COL32( c->col[ 0 ], c->col[ 1 ], c->col[ 2 ], c->col[ 3 ] );
    draw_list->AddRectFilled( p_min, p_max, col, c->rounding, ctxCmd->flags );
    draw_list->AddText(p_txt, IM_COL32_BLACK, c->text);
}

const fnDraw COMMANDS[] = { &cmdNone, &cmdDrawRect, &cmdDrawCircle };

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
    ImDrawFlags flags = 0;
    CommandContext ctxCmd = { offset, flags };

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
