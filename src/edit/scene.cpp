#include "scene.h"

#include "appConfig.h"
#include "common.h"

#include "pxr/pxr.h"

#include "pxr/base/gf/bbox3d.h"
#include "pxr/base/gf/frustum.h"
#include "pxr/base/gf/matrix4d.h"
#include "pxr/base/gf/matrix4f.h"
#include "pxr/base/gf/range3d.h"
#include "pxr/base/gf/rotation.h"

#include "pxr/usd/usdGeom/bboxCache.h"
#include "pxr/usd/usdGeom/metrics.h"

#include <ecl/ecl.h>

#include "base/mopr.h"

#include "wrap/usd/box/layer.h"

// TF_VERIFY calls don't compile without the using directive.
PXR_NAMESPACE_USING_DIRECTIVE

namespace mopr
{

Scene::Scene( const std::string & usdsPath,
              const char * camera,
              float pixelsW,
              float pixelsH )
    : stage( ), camera( ), drawTarget( ), lighting( ), renderSettings( )
{
    this->commandOptions.nofOptions = 0;
    this->commandOptions.options = NULL;
    this->commandQueue.nofCommands = 0;
    this->commandQueue.commands = NULL;
    this->commandQueue.pixelsW = pixelsW;
    this->commandQueue.pixelsH = pixelsH;
    this->initStageAndCamera( usdsPath, camera );
    // mopr_print_command_queue( &this->commandQueue );
}

static cl_object
 getSymbol( const char * symName, cl_object pkg_l )
{
    cl_object strSym_l = ecl_make_constant_base_string( symName, -1 );
    int flags = 0;
    return ecl_find_symbol( strSym_l, pkg_l, &flags );
}

static bool
 destructCommandQueue( CommandQueue * queue )
{
    cl_object hQueue_l = ecl_make_pointer( queue );
    cl_object pkgMoprExtRepr_l = ecl_find_package( "MOPR-EXT/REPR" );
    cl_object symFn_l = getSymbol( "DESTRUCT-COMMAND-QUEUE", pkgMoprExtRepr_l );
    cl_funcall( 2, symFn_l, hQueue_l );
    return true;
}

static bool
 destructCommandOptions( CommandOptions * options )
{
    cl_object hOptions_l = ecl_make_pointer( options );
    cl_object pkgMoprExtRepr_l = ecl_find_package( "MOPR-EXT/REPR" );
    cl_object symFn_l = getSymbol( "DESTRUCT-COMMAND-OPTIONS", pkgMoprExtRepr_l );
    cl_funcall( 2, symFn_l, hOptions_l );
    return true;
}

Scene::~Scene( )
{
    destructCommandQueue( &this->commandQueue );
    destructCommandOptions( &this->commandOptions );
}

void
 Scene::frameAll( double viewTranslate[ 3 ] )
{
    auto const & appConfig = mopr::AppConfig::GetInstance( );

    pxr::TfTokenVector purposes;
    purposes.push_back( pxr::UsdGeomTokens->default_ );
    if ( appConfig.enablePurposeGuide )
    {
        purposes.push_back( pxr::UsdGeomTokens->guide );
    }
    if ( appConfig.enablePurposeProxy )
    {
        purposes.push_back( pxr::UsdGeomTokens->proxy );
    }
    if ( appConfig.enablePurposeRender )
    {
        purposes.push_back( pxr::UsdGeomTokens->render );
    }

    pxr::UsdGeomBBoxCache bboxCache( pxr::UsdTimeCode::EarliestTime( ), purposes, true );
    pxr::GfBBox3d bbox = bboxCache.ComputeWorldBound( this->stage->GetPseudoRoot( ) );
    pxr::GfRange3d world = bbox.ComputeAlignedRange( );

    pxr::GfVec3d worldCenter = ( world.GetMin( ) + world.GetMax( ) ) / 2.0;
    double worldSize = world.GetSize( ).GetLength( );

    if ( pxr::UsdGeomGetStageUpAxis( this->stage ) == pxr::UsdGeomTokens->z )
    {
        viewTranslate[ 0 ] = -worldCenter[ 0 ];
        viewTranslate[ 1 ] = -worldCenter[ 2 ];
        viewTranslate[ 2 ] = -worldCenter[ 1 ] - worldSize;
    }
    else
    {
        viewTranslate[ 0 ] = -worldCenter[ 0 ];
        viewTranslate[ 1 ] = -worldCenter[ 1 ];
        viewTranslate[ 2 ] = -worldCenter[ 2 ] - worldSize;
    }
}

bool
 Scene::init( const AppState * appState )
{
    this->initDrawTarget( appState );
    this->initEngine( );
    this->initLighting( appState );

    return true;
}

static bool
 readLispFile( SdfLayerRefPtr layer,
               CommandQueue * queue,
               const std::string & resolvedPath,
               bool callEnabled = false )
{
    MoprLayer sLayer;
    sLayer.SetRefPtr( layer );
    cl_object hLayer_l = ecl_make_pointer( &sLayer );
    cl_object hQueue_l = ecl_make_pointer( queue );
    cl_object pkgMoprExtUtil_l = ecl_find_package( "MOPR-EXT/UTIL" );
    cl_object symFn_l =
     getSymbol( "POPULATE-FROM-LISP-FILE-WITH-REPR", pkgMoprExtUtil_l );
    cl_object strFileName_l = ecl_make_constant_base_string( resolvedPath.c_str( ), -1 );
    cl_funcall(
     5, symFn_l, hLayer_l, hQueue_l, strFileName_l, callEnabled ? ECL_T : ECL_NIL );
    if ( !layer ) return false;

    return true;
}

void
 Scene::initStageAndCamera( const std::string & usdsPath, const char * camera )
{
    pxr::SdfLayerRefPtr layer = pxr::SdfLayer::CreateAnonymous( );
    int result =
     readLispFile( layer, &this->commandQueue, usdsPath, /* callEnabled = */ true );
    ( void ) result;
    this->stage = pxr::UsdStage::Open( layer, pxr::UsdStage::LoadAll );
    if ( camera ) this->camera = pxr::SdfPath( camera );
}

static bool
 populateCommandOptions( CommandOptions * opts, unsigned int id, unsigned int idSub )
{
    cl_object id_l = ecl_make_unsigned_integer( id );
    cl_object idSub_l = ecl_make_unsigned_integer( idSub );
    cl_object hOpts_l = ecl_make_pointer( opts );
    cl_object pkgMoprExtUtil_l = ecl_find_package( "MOPR-EXT/UTIL" );
    cl_object symFn_l = getSymbol( "POPULATE-COMMAND-OPTIONS", pkgMoprExtUtil_l );
    cl_funcall( 4, symFn_l, hOpts_l, id_l, idSub_l );
    return true;
}

void
 Scene::getCommandOptions( unsigned int id, unsigned int idSub )
{
    populateCommandOptions( &this->commandOptions, id, idSub );
    // mopr_print_command_options( &this->commandOptions );
}

void
 Scene::resetCommandOptions( )
{
    destructCommandOptions( &this->commandOptions );
}

static bool
 applyOptionInternal( unsigned int id, unsigned int idSub, unsigned int idOpt )
{
    cl_object id_l = ecl_make_unsigned_integer( id );
    cl_object idSub_l = ecl_make_unsigned_integer( idSub );
    cl_object idOpt_l = ecl_make_unsigned_integer( idOpt );
    cl_object pkgMoprExtUtil_l = ecl_find_package( "MOPR-EXT/UTIL" );
    cl_object symFn_l = getSymbol( "APPLY-COMMAND-OPTION", pkgMoprExtUtil_l );
    cl_funcall( 4, symFn_l, id_l, idSub_l, idOpt_l );
    return true;
}

void
 Scene::applyOption( unsigned int id, unsigned int idSub, unsigned int idOpt )
{
    applyOptionInternal( id, idSub, idOpt );
}

void
 Scene::initDrawTarget( const AppState * appState )
{
    // Should be called after initializing the GL API.
    this->drawTarget = pxr::GlfDrawTarget::New(
     pxr::GfVec2i( appState->screenW, appState->screenH ), false );
    this->drawTarget->Bind( );
    this->drawTarget->AddAttachment( "color", GL_RGBA, GL_FLOAT, GL_RGBA );
    this->drawTarget->AddAttachment(
     "depth", GL_DEPTH_COMPONENT, GL_FLOAT, GL_DEPTH_COMPONENT );
    this->drawTarget->Unbind( );
}

void
 Scene::initEngine( )
{
    auto const & appConfig = mopr::AppConfig::GetInstance( );

    pxr::UsdImagingGLEngine::Parameters parameters;
    parameters.rootPath = this->stage->GetPseudoRoot( ).GetPath( );
    parameters.displayUnloadedPrimsWithBounds = false;

    this->engine = std::make_shared< pxr::UsdImagingGLEngine >( parameters );

    if ( !appConfig.renderer.empty( ) )
    {
        const pxr::TfToken rendererName{ appConfig.renderer };
        if ( !this->engine->SetRendererPlugin( rendererName ) )
        {
            std::cerr << "Couldn't set renderer plugin: " << appConfig.renderer
                      << std::endl;
            exit( -1 );
        }
        else
        {
            std::cout << "Renderer plugin: " << appConfig.renderer << std::endl;
        }
    }

    for ( const auto & renderSetting : this->renderSettings )
    {
        this->engine->SetRendererSetting( pxr::TfToken( renderSetting.first ),
                                          renderSetting.second );
    }
}

void
 Scene::initLighting( const AppState * appState )
{
    auto const & appConfig = mopr::AppConfig::GetInstance( );

    if ( appConfig.enableLightingPlaceholder )
    {
        this->lighting = pxr::GlfSimpleLightingContext::New( );
        // Set same parameter as GlfSimpleLightingContext::SetStateFromOpenGL
        // OpenGL defaults.
        if ( !appConfig.enableLightingScene )
        {
            pxr::GlfSimpleLight light;
            if ( appConfig.enableLightingCamera )
            {
                light.SetPosition( pxr::GfVec4f( appState->viewTranslate[ 0 ],
                                                 appState->viewTranslate[ 2 ],
                                                 appState->viewTranslate[ 1 ],
                                                 0 ) );
            }
            else
            {
                light.SetPosition( pxr::GfVec4f( 0, -.5, .5, 0 ) );
            }
            light.SetDiffuse( pxr::GfVec4f( 1, 1, 1, 1 ) );
            light.SetAmbient( pxr::GfVec4f( 0, 0, 0, 1 ) );
            light.SetSpecular( pxr::GfVec4f( 1, 1, 1, 1 ) );
            pxr::GlfSimpleLightVector lights;
            lights.push_back( light );
            this->lighting->SetLights( lights );
        }

        pxr::GlfSimpleMaterial material;
        material.SetAmbient( pxr::GfVec4f( 0.2, 0.2, 0.2, 1.0 ) );
        material.SetDiffuse( pxr::GfVec4f( 0.8, 0.8, 0.8, 1.0 ) );
        material.SetSpecular( pxr::GfVec4f( 0, 0, 0, 1 ) );
        material.SetShininess( 0.0001f );
        this->lighting->SetMaterial( material );
        this->lighting->SetSceneAmbient( pxr::GfVec4f( 0.2, 0.2, 0.2, 1.0 ) );
    }
}

void
 Scene::draw( double frame, const AppState * appState )
{
    auto const & appConfig = mopr::AppConfig::GetInstance( );

    this->drawTarget->Bind( );

    // Update the draw target's size.
    this->drawTarget->SetSize( pxr::GfVec2i( appState->screenW, appState->screenH ) );

    GLfloat clearColor[ 4 ] = { 0.1, 0, 0, 0 };
    GLfloat clearDepth[ 1 ] = { 1 };

    glClearBufferfv( GL_COLOR, 0, clearColor );
    glClearBufferfv( GL_DEPTH, 0, clearDepth );

    if ( this->camera.IsEmpty( ) )
    {
        pxr::GfMatrix4d viewMatrix( 1.0 );
        viewMatrix *= pxr::GfMatrix4d( ).SetRotate(
         pxr::GfRotation( pxr::GfVec3d( 0, 1, 0 ), appState->viewRotate[ 0 ] ) );
        viewMatrix *= pxr::GfMatrix4d( ).SetRotate(
         pxr::GfRotation( pxr::GfVec3d( 1, 0, 0 ), appState->viewRotate[ 1 ] ) );
        viewMatrix *=
         pxr::GfMatrix4d( ).SetTranslate( pxr::GfVec3d( appState->viewTranslate[ 0 ],
                                                        appState->viewTranslate[ 1 ],
                                                        appState->viewTranslate[ 2 ] ) );

        pxr::GfMatrix4d modelViewMatrix = viewMatrix;
        if ( pxr::UsdGeomGetStageUpAxis( this->stage ) == pxr::UsdGeomTokens->z )
        {
            modelViewMatrix = pxr::GfMatrix4d( ).SetRotate(
                               pxr::GfRotation( pxr::GfVec3d( 1.0, 0.0, 0.0 ), -90.0 ) )
                              * modelViewMatrix;
        }

        const double aspectRatio = double( appState->screenW ) / appState->screenH;
        pxr::GfFrustum frustum;
        frustum.SetPerspective( 60.0, aspectRatio, 1, 100000.0 );
        const pxr::GfMatrix4d projMatrix = frustum.ComputeProjectionMatrix( );

        this->engine->SetCameraState( modelViewMatrix, projMatrix );
    }
    else
    {
        this->engine->SetCameraPath( this->camera );
    }

    this->engine->SetOverrideWindowPolicy( pxr::CameraUtilFit );

    pxr::GfRange2f displayWindow = {
        pxr::GfVec2f( 0, 0 ), pxr::GfVec2f( 0 + appState->screenW, 0 + appState->screenH )
    };
    pxr::GfRect2i dataWindow = { pxr::GfVec2i( 0, 0 ),
                                 appState->screenW,
                                 appState->screenH };
    float pixelAspectRatio = 1.0f;
    const CameraUtilFraming framing( displayWindow, dataWindow, pixelAspectRatio );
    if ( framing.IsValid( ) )
    {
        this->engine->SetRenderBufferSize(
         pxr::GfVec2i( appState->screenW, appState->screenH ) );
        this->engine->SetFraming( framing );
    }
    else
    {
        const pxr::GfVec4d viewport( 0, 0, appState->screenW, appState->screenH );
        this->engine->SetRenderViewport( viewport );
    }

    pxr::UsdImagingGLRenderParams params;
    // params.drawMode = pxr::UsdImagingGLDrawMode::DRAW_SHADED_SMOOTH;
    params.drawMode = pxr::UsdImagingGLDrawMode::DRAW_WIREFRAME;
    params.enableLighting = appConfig.enableLightingPlaceholder;
    params.enableIdRender = false;
    params.enableSceneMaterials = appConfig.enableSceneMaterials;
    params.complexity = appConfig.complexity;
    params.cullStyle = pxr::UsdImagingGLCullStyle::CULL_STYLE_NOTHING;
    params.showGuides = appConfig.enablePurposeGuide;
    params.showProxy = appConfig.enablePurposeProxy;
    params.showRender = appConfig.enablePurposeRender;
    params.clearColor = pxr::GfVec4f{ 1.0f, 0.5f, 0.1f, 1.0f };
    params.frame = pxr::UsdTimeCode( frame );
    // params.clipPlanes = std::vector<GfVec4d>{};

    this->engine->SetRendererAov( pxr::HdAovTokens->color );

    if ( appConfig.enableLightingPlaceholder )
    {
        this->engine->SetLightingState( this->lighting );
    }

    static const int maxIterations = 100;
    pxr::TfErrorMark mark;
    int convergenceIterations = 0;
    do
    {
        convergenceIterations++;
        this->engine->Render( this->stage->GetPseudoRoot( ), params );
    } while ( !this->engine->IsConverged( ) && convergenceIterations <= maxIterations );

    glFinish( );

    TF_VERIFY( mark.IsClean( ), "Errors occurred while rendering." );

    this->drawTarget->Unbind( );
}

}   // namespace mopr
