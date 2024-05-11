#include "menu.h"

#include "imgui.h"

namespace mopr
{

void
 Menu::draw( )
{
    ImGuiViewport * viewport = ImGui::GetMainViewport( );
    ImGui::SetNextWindowPos( viewport->Pos );
    ImGui::SetNextWindowSize( ImVec2( viewport->Size.x / 2, viewport->Size.y ) );
    ImGui::SetNextWindowViewport( viewport->ID );

    ImGuiWindowFlags windowFlags =
     ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_NoTitleBar
     | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove
     | ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoNavFocus
     // | ImGuiWindowFlags_NoBackground
     ;

    if ( !ImGui::Begin( "Mopr Editor", nullptr, windowFlags ) )
    {
        ImGui::End( );
        return;
    }

    ImGui::Text( "Mopr Editor ..." );

    ImGui::End( );
}

}   // namespace mopr
