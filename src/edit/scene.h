#ifndef MOPR_MAIN_SCENE_H
#define MOPR_MAIN_SCENE_H

#include "appState.h"

#include "core/command.h"

#include "pxr/base/gf/vec3d.h"

#include "pxr/usd/usd/stage.h"

#include "pxr/usd/usdGeom/tokens.h"

#include "pxr/imaging/cameraUtil/framing.h"

#include "pxr/imaging/glf/drawTarget.h"
#include "pxr/imaging/glf/simpleLightingContext.h"

#include "pxr/imaging/hd/tokens.h"

#include "pxr/usdImaging/usdImaging/tokens.h"

#include "pxr/usdImaging/usdImagingGL/engine.h"

namespace mopr
{

//
// Scene
//

struct Scene
{
    std::shared_ptr< class pxr::UsdImagingGLEngine > engine;
    // Stage and camera path are directly related.
    pxr::UsdStageRefPtr stage;
    pxr::SdfPath camera;

    pxr::GlfDrawTargetRefPtr drawTarget;
    pxr::GlfSimpleLightingContextRefPtr lighting;

    pxr::VtDictionary renderSettings;

    // Populated and cleaned up on the Lisp side.
    CommandQueue commandQueue;
    CommandOptions commandOptions;

    Scene( const std::string & usdsPath,
           const char * camera,
           float pixelsW,
           float pixelsH );

    ~Scene( );

    void
     initStageAndCamera( const std::string & usdsPath, const char * camera );

    void
     getCommandOptions( unsigned int id, unsigned int idSub );

    void
     resetCommandOptions( );

    void
     applyOption( unsigned int id, unsigned int idSub, unsigned int idOpt );

    void
     frameAll( double viewTranslate[ 3 ] );

    void
     initDrawTarget( const AppState * appState );
    void
     initEngine( );
    void
     initLighting( const AppState * appState );

    bool
     init( const AppState * appState );

    void
     draw( double frame, const AppState * appState );
};

}   // namespace mopr

#endif   // MOPR_MAIN_SCENE_H
