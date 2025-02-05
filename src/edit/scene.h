#ifndef MOPR_MAIN_SCENE_H
#define MOPR_MAIN_SCENE_H

#include "appState.h"

#include "repr/command.h"

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

class Scene
{
    std::shared_ptr< class pxr::UsdImagingGLEngine > engine;
    // Stage, frame step and camera path are directly related.
    pxr::UsdStageRefPtr stage;
    double fstepMS;
    pxr::SdfPath camera;

    pxr::GlfDrawTargetRefPtr drawTarget;
    pxr::GlfSimpleLightingContextRefPtr lighting;

    pxr::VtDictionary renderSettings;

  public:
    Scene( const pxr::SdfLayerRefPtr layer, const std::string & cameraPath );

    ~Scene( );

    void
     frameAll( double viewTranslate[ 3 ] ) const;

    double
     frameStepMS( ) const
    {
        return this->fstepMS;
    }

    GLint
     fboDrawTarget( ) const
    {
        return this->drawTarget->GetFramebufferId( );
    }

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
