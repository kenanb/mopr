#include "appState.h"

namespace mopr
{

AppState::AppState( const AppEnvironment * appEnv, const AppConfig * appCfg )

    // Defaulted to values from AppEnvironment.
    : projectPath( appEnv->getProjectPath( ) )
    , frameFirst( appEnv->frameFirst )
    , frameLast( appEnv->frameLast )

    // Defaulted to values from AppConfig.
    , screenW( appCfg->screenW )
    , screenH( appCfg->screenH )
    , viewRotate{ appCfg->viewRotate[ 0 ], appCfg->viewRotate[ 1 ] }
    , viewTranslate{ appCfg->viewTranslate[ 0 ],
                     appCfg->viewTranslate[ 1 ],
                     appCfg->viewTranslate[ 2 ] }

    // Defaulted to values from constructor.
    , quit( false )
    , showOverlays( false )
    , nav( NAVIGATION_STATE_NONE )
    , mx( 0.0 )
    , my( 0.0 )
    , assetPath( )

{
}

}   // namespace mopr
