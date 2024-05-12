#include "editor.h"

#include "imgui.h"

#include <yoga/Yoga.h>

namespace mopr
{

static void
 dummyTree( );

void
 Editor::draw( )
{
    ImGuiViewport * viewport = ImGui::GetMainViewport( );
    ImGui::SetNextWindowPos( ImVec2( viewport->Pos.x + 10, viewport->Pos.y + 10 ) );
    ImGui::SetNextWindowSize(
     ImVec2( viewport->Size.x / 2 + 20, viewport->Size.y - 20 ) );
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

    ImGui::Text( "..." );

    dummyTree( );

    for ( int i = 0; i < 40; i++ ) ImGui::Text( "..." );

    ImGui::End( );
}

void
 dummyTree( )
{
    // For disabling pixel rounding:
    // YGConfigRef config = YGConfigNew( );
    // YGConfigSetPointScaleFactor( config, 0.0f );

    const float pixelsW = 640;
    const float pixelsH = 480;
    YGNodeRef root = YGNodeNew( );
    YGNodeStyleSetFlexDirection( root, YGFlexDirectionColumn );
    YGNodeStyleSetWidth( root, pixelsW );
    YGNodeStyleSetHeight( root, pixelsH );
    YGNodeStyleSetPadding( root, YGEdgeAll, 10.0f );

    YGNodeRef c0 = YGNodeNew( );
    YGNodeStyleSetFlexGrow( c0, 1.0f );
    YGNodeStyleSetMargin( c0, YGEdgeAll, 10.0f );

    YGNodeRef c1 = YGNodeNew( );
    YGNodeStyleSetFlexGrow( c1, 1.0f );
    YGNodeStyleSetMargin( c1, YGEdgeAll, 10.0f );

    YGNodeStyleSetFlexDirection( c1, YGFlexDirectionRow );
    YGNodeRef c1c0 = YGNodeNew( );
    YGNodeStyleSetFlexGrow( c1c0, 1.0f );
    YGNodeStyleSetMargin( c1c0, YGEdgeAll, 10.0f );
    YGNodeRef c1c1 = YGNodeNew( );
    YGNodeStyleSetFlexGrow( c1c1, 1.0f );
    YGNodeStyleSetMargin( c1c1, YGEdgeAll, 10.0f );
    YGNodeRef c1c2 = YGNodeNew( );
    YGNodeStyleSetFlexGrow( c1c2, 1.0f );
    YGNodeStyleSetMargin( c1c2, YGEdgeAll, 10.0f );

    YGNodeInsertChild( root, c0, 0 );
    YGNodeInsertChild( root, c1, 1 );
    YGNodeInsertChild( c1, c1c0, 0 );
    YGNodeInsertChild( c1, c1c1, 1 );
    YGNodeInsertChild( c1, c1c2, 2 );

    YGNodeCalculateLayout( root, YGUndefined, YGUndefined, YGDirectionLTR );

    float Lr = YGNodeLayoutGetLeft( root );
    float Rr = YGNodeLayoutGetRight( root );
    float Tr = YGNodeLayoutGetTop( root );
    float Br = YGNodeLayoutGetBottom( root );
    float Wr = YGNodeLayoutGetWidth( root );
    float Hr = YGNodeLayoutGetHeight( root );

    float Lc0 = YGNodeLayoutGetLeft( c0 );
    float Rc0 = YGNodeLayoutGetRight( c0 );
    float Tc0 = YGNodeLayoutGetTop( c0 );
    float Bc0 = YGNodeLayoutGetBottom( c0 );
    float Wc0 = YGNodeLayoutGetWidth( c0 );
    float Hc0 = YGNodeLayoutGetHeight( c0 );

    float Lc1 = YGNodeLayoutGetLeft( c1 );
    float Rc1 = YGNodeLayoutGetRight( c1 );
    float Tc1 = YGNodeLayoutGetTop( c1 );
    float Bc1 = YGNodeLayoutGetBottom( c1 );
    float Wc1 = YGNodeLayoutGetWidth( c1 );
    float Hc1 = YGNodeLayoutGetHeight( c1 );

    float Lc1c0 = YGNodeLayoutGetLeft( c1c0 );
    // float Rc1c0 = YGNodeLayoutGetRight( c1c0 );
    float Tc1c0 = YGNodeLayoutGetTop( c1c0 );
    // float Bc1c0 = YGNodeLayoutGetBottom( c1c0 );
    float Wc1c0 = YGNodeLayoutGetWidth( c1c0 );
    float Hc1c0 = YGNodeLayoutGetHeight( c1c0 );

    float Lc1c1 = YGNodeLayoutGetLeft( c1c1 );
    // float Rc1c1 = YGNodeLayoutGetRight( c1c1 );
    float Tc1c1 = YGNodeLayoutGetTop( c1c1 );
    // float Bc1c1 = YGNodeLayoutGetBottom( c1c1 );
    float Wc1c1 = YGNodeLayoutGetWidth( c1c1 );
    float Hc1c1 = YGNodeLayoutGetHeight( c1c1 );

    float Lc1c2 = YGNodeLayoutGetLeft( c1c2 );
    // float Rc1c2 = YGNodeLayoutGetRight( c1c2 );
    float Tc1c2 = YGNodeLayoutGetTop( c1c2 );
    // float Bc1c2 = YGNodeLayoutGetBottom( c1c2 );
    float Wc1c2 = YGNodeLayoutGetWidth( c1c2 );
    float Hc1c2 = YGNodeLayoutGetHeight( c1c2 );

    YGNodeFreeRecursive( root );

    ImDrawList * draw_list = ImGui::GetWindowDrawList( );

    // Get the current ImGui cursor position
    ImVec2 p = ImGui::GetCursorScreenPos( );

    float rounding = 10.0f;
    ImDrawFlags flags = 0;

    {
        ImVec2 p_min = { p.x + Lc0, p.y + Tc0 };
        ImVec2 p_max = { p.x + Lc0 + Wc0, p.y + Tc0 + Hc0 };
        ImU32 col = IM_COL32( 255, 255, 255, 50 );
        draw_list->AddRectFilled( p_min, p_max, col, rounding, flags );
    }

    {
        ImVec2 p_min = { p.x + Lc1, p.y + Tc1 };
        ImVec2 p_max = { p.x + Lc1 + Wc1, p.y + Tc1 + Hc1 };
        ImU32 col = IM_COL32( 255, 255, 255, 50 );
        draw_list->AddRectFilled( p_min, p_max, col, rounding, flags );
    }

    {
        ImVec2 p_min = { p.x + Lc1 + Lc1c0, p.y + Tc1 + Tc1c0 };
        ImVec2 p_max = { p.x + Lc1 + Lc1c0 + Wc1c0, p.y + Tc1 + Tc1c0 + Hc1c0 };
        ImU32 col = IM_COL32( 255, 100, 100, 50 );
        draw_list->AddRectFilled( p_min, p_max, col, rounding, flags );
    }

    {
        ImVec2 p_min = { p.x + Lc1 + Lc1c1, p.y + Tc1 + Tc1c1 };
        ImVec2 p_max = { p.x + Lc1 + Lc1c1 + Wc1c1, p.y + Tc1 + Tc1c1 + Hc1c1 };
        ImU32 col = IM_COL32( 255, 100, 100, 50 );
        draw_list->AddRectFilled( p_min, p_max, col, rounding, flags );
    }

    {
        ImVec2 p_min = { p.x + Lc1 + Lc1c2, p.y + Tc1 + Tc1c2 };
        ImVec2 p_max = { p.x + Lc1 + Lc1c2 + Wc1c2, p.y + Tc1 + Tc1c2 + Hc1c2 };
        ImU32 col = IM_COL32( 255, 100, 100, 50 );
        draw_list->AddRectFilled( p_min, p_max, col, rounding, flags );
    }

    // Advance the ImGui cursor to claim space in the window (otherwise the window will appear small and needs to be resized)
    ImGui::Dummy( ImVec2( pixelsW, pixelsH ) );
}

}   // namespace mopr
