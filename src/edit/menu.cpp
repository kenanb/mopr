#include "menu.h"

#include "imgui.h"

namespace mopr
{

void
 Menu::draw( )
{
    ImGui::Begin( "Mopr Editor" );

    ImGui::Text( "Mopr Editor ..." );

    ImGui::End( );
}

}   // namespace mopr
