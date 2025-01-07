#include "editor.h"

#include "repr/command.h"

namespace mopr
{

void
 Editor::draw( CommandQueue const & q,
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

    for ( const auto & cmd : q.commands )
    {
        if ( cmd->apply( draw_list, &ctxCmd ) )
        {
            if ( ctxCmd.idSelected == cmd->idNode && ctxCmd.idSubSelected == cmd->idSub )
            {
                ctxCmd.idSelected = 0;
                ctxCmd.idSubSelected = 0;
            }
            else
            {
                ctxCmd.idSelected = cmd->idNode;
                ctxCmd.idSubSelected = cmd->idSub;
            }
        }
    }

    *idSelected = ctxCmd.idSelected;
    *idSubSelected = ctxCmd.idSubSelected;

    // Advance the ImGui cursor to claim space. If the "reserved" height and width were
    // not fully used, we expect the values to have already been adjusted to "used" area.
    ImGui::SetCursorScreenPos( ImVec2( offset.x + q.pixelsW, offset.y + q.pixelsH ) );

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
 Editor::drawOptions( const std::vector< std::string > & o, unsigned int * optSelected )
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

    for ( int i = 0; i < o.size( ); i++ )
    {
        unsigned int idx = i + 1;
        if ( drawOption( draw_list, o[ i ].c_str( ), idx, &ctxCmd, *optSelected ) )
        {
            *optSelected = idx;
        }
    }

    // Advance the ImGui cursor to claim space. If the "reserved" height and width were
    // not fully used, we expect the values to have already been adjusted to "used" area.
    ImGui::SetCursorScreenPos( ImVec2( offset.x + 300, offset.y + ( o.size( ) * 40 ) ) );

    ImGui::End( );
}

}   // namespace mopr
