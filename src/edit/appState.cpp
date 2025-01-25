#include "appState.h"

namespace mopr
{

AppState::AppState( int screenW, int screenH )
    : screenW( screenW )
    , screenH( screenH )
    , quit( false )
    , showOverlays( false )
    , nav( NAVIGATION_STATE_NONE )
    , idSelected( 0 )
    , idSubSelected( 0 )
    , mx( 0.0 )
    , my( 0.0 )
{
    viewRotate[ 0 ] = 0.0;
    viewRotate[ 1 ] = 0.0;
    viewTranslate[ 0 ] = 0.0;
    viewTranslate[ 1 ] = 0.0;
    viewTranslate[ 2 ] = 0.0;
}

}   // namespace mopr
