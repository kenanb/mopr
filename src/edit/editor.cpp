#include "editor.h"

#include "repr/command.h"

namespace mopr
{

static const int midPointOffsetX = 44;
static const int padding = 4;

void
 Editor::drawMenu( const std::map< std::string, std::string > & assetPaths,
                   std::string & assetSelected ) const
{
    if ( ImGui::BeginMainMenuBar( ) )
    {
        ImGui::Text( "MOPR EDITOR" );
        ImGui::Separator( );

        if ( ImGui::BeginMenu( "Project" ) )
        {
            if ( ImGui::BeginMenu( "Assets" ) )
            {
                for ( auto const & assetPathPair : assetPaths )
                {
                    if ( ImGui::MenuItem( assetPathPair.first.c_str( ),
                                          assetPathPair.second.c_str( ) ) )
                    {
                        assetSelected = assetPathPair.first;
                    }
                }
                ImGui::EndMenu( );
            }
            ImGui::EndMenu( );
        }
        if ( ImGui::BeginMenu( "Panels", false ) )
        {
            ImGui::EndMenu( );
        }
        ImGui::EndMainMenuBar( );
    }
}

void
 Editor::drawParameters( ) const
{
    ImGuiViewport * viewport = ImGui::GetMainViewport( );
    int windowWidth = 256;
    int windowHeight = 256;
    ImGui::SetNextWindowSize( ImVec2( windowWidth, windowHeight ) );
    ImGui::SetNextWindowViewport( viewport->ID );

    ImGuiWindowFlags windowFlags =   //
     ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoBackground;

    if ( !ImGui::Begin( "PARAMETERS", nullptr, windowFlags ) )
    {
        ImGui::End( );
        return;
    }

    ImGui::Text( "..." );

    ImGui::End( );
}

void
 Editor::drawContents( ) const
{
    ImGuiViewport * viewport = ImGui::GetMainViewport( );
    int windowWidth = 256;
    int windowHeight = 256;
    ImGui::SetNextWindowSize( ImVec2( windowWidth, windowHeight ) );
    ImGui::SetNextWindowViewport( viewport->ID );

    ImGuiWindowFlags windowFlags =   //
     ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoBackground;

    if ( !ImGui::Begin( "CONTENTS", nullptr, windowFlags ) )
    {
        ImGui::End( );
        return;
    }

    ImGui::Text( "..." );

    ImGui::End( );
}

void
 Editor::drawMain( ) const
{
    ImGuiViewport * viewport = ImGui::GetMainViewport( );
    int windowWidth = viewport->Size.x / 2 - midPointOffsetX - ( padding * 3 );
    int windowHeight = viewport->Size.y - ImGui::GetFrameHeight( ) - ( padding * 4 );
    ImGui::SetNextWindowPos(
     ImVec2( viewport->Pos.x + ( padding * 2 ),
             viewport->Pos.y + ImGui::GetFrameHeight( ) + ( padding * 2 ) ) );
    ImGui::SetNextWindowSize( ImVec2( windowWidth, windowHeight ) );
    ImGui::SetNextWindowViewport( viewport->ID );

    ImGuiWindowFlags windowFlags =   //
     ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_NoScrollbar
     | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoResize
     | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoBringToFrontOnFocus
     | ImGuiWindowFlags_NoNavFocus;

    ImGui::PushStyleVar( ImGuiStyleVar_WindowPadding, ImVec2( 0.0f, 0.0f ) );
    // NOTE: We don't early out when window is collapsed, because it is the
    // dockspace container. ImGui demo suggests this is necessary to keep the
    // docking relationship with the docked windows.
    ImGui::Begin( "PANELS", nullptr, windowFlags );
    ImGui::PopStyleVar( );

    ImGuiDockNodeFlags dockspaceFlags = ImGuiDockNodeFlags_NoResize;

    ImGuiID dockspaceId = ImGui::GetID( "MainDockspace" );
    ImGui::DockSpace(
     dockspaceId, ImVec2( windowWidth, windowHeight - 24 ), dockspaceFlags );
    ImGui::SetNextWindowDockID( dockspaceId );

    ImGui::End( );

    ImGui::SetNextWindowDockID( dockspaceId, ImGuiCond_Always );
    drawParameters( );

    ImGui::SetNextWindowDockID( dockspaceId, ImGuiCond_Once );
    drawContents( );
}

void
 Editor::drawTree( CommandQueue const & commandQueue,
                   std::vector< std::string > const & payloadOptions,
                   unsigned int * idSelected,
                   unsigned int * idSubSelected,
                   unsigned int * optSelected ) const
{
    ImGuiViewport * viewport = ImGui::GetMainViewport( );
    int windowWidth = viewport->Size.x / 2 + midPointOffsetX - ( padding * 3 );
    int windowHeight = viewport->Size.y - ImGui::GetFrameHeight( ) - ( padding * 4 );
    ImGui::SetNextWindowPos( ImVec2(
     viewport->Pos.x + viewport->Size.x - windowWidth - ( padding * 2 ),   // Right align.
     viewport->Pos.y + ImGui::GetFrameHeight( ) + ( padding * 2 ) ) );
    ImGui::SetNextWindowSize( ImVec2( windowWidth, windowHeight ) );
    ImGui::SetNextWindowViewport( viewport->ID );

    ImGuiWindowFlags windowFlags =   //
     ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_NoSavedSettings
     | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove
     | ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoNavFocus
     | ImGuiWindowFlags_MenuBar;

    if ( !ImGui::Begin( "PROCEDURE", nullptr, windowFlags ) )
    {
        ImGui::End( );
        return;
    }

    if ( ImGui::BeginMenuBar( ) )
    {
        const std::vector< std::string > nodeOptions;
        if ( ImGui::BeginMenu( "Node Actions", !nodeOptions.empty( ) ) )
        {
            ImGui::EndMenu( );
        }

        if ( ImGui::BeginMenu( "Paylod Actions", !payloadOptions.empty( ) ) )
        {
            for ( int i = 0; i < payloadOptions.size( ); i++ )
            {
                if ( ImGui::MenuItem( payloadOptions[ i ].c_str( ) ) )
                    *optSelected = i + 1;
            }
            ImGui::EndMenu( );
        }
        ImGui::EndMenuBar( );
    }

    ImDrawList * draw_list = ImGui::GetWindowDrawList( );

    // Get the current ImGui cursor position
    ImVec2 offset = ImGui::GetCursorScreenPos( );
    CommandContext ctxCmd{ *idSelected, *idSubSelected, offset };

    for ( const auto & cmd : commandQueue.commands )
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
    ImGui::SetCursorScreenPos(
     ImVec2( offset.x + commandQueue.pixelsW, offset.y + commandQueue.pixelsH ) );

    ImGui::End( );
}

}   // namespace mopr
