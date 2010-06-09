{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards, DoRec #-}

module Graphics.Formats.Collada.Render 
    ( compile )
where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Formats.Collada.Objects as O
import qualified Data.Map as Map
import qualified Foreign.Marshal.Array as Array
import qualified Foreign.Storable as Storable
import Foreign.Ptr (Ptr)
import Data.Monoid (Monoid(..))
import Control.Arrow (second)
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad (when, forM_, liftM2, liftM3)
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)

newtype DrawM a = DrawM { runDrawM :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

wrapDrawM :: (IO a -> IO a) -> (DrawM a -> DrawM a)
wrapDrawM f = DrawM . f . runDrawM

type Bindings = Map.Map String (DrawM ())

type Cache = Map.Map O.ID Any

data CompileEnv = CompileEnv {
    envDict :: O.Dict,
    envBindings :: Bindings,
    envLoader :: String -> IO GL.TextureObject
}

newtype CompileM a = CompileM { runCompileM :: ReaderT CompileEnv (StateT Cache IO) a }
    deriving (Functor, Applicative, Monad, MonadIO)

cached :: (O.Object -> CompileM a) -> (O.ID -> CompileM a)
cached f ident = CompileM $ do
    cache <- lift get
    case Map.lookup ident cache of
        Nothing -> do
                rec
                    lift . modify . Map.insert ident . unsafeCoerce $ result
                    result <- runCompileM $ f =<< findSymbol ident
                return result
        Just result -> return (unsafeCoerce result)

lookup' :: (Ord k, Show k) => k -> Map.Map k a -> a
lookup' k mp | Just x <- Map.lookup k mp = x
             | otherwise                 = error $ "Couldn't find object: " ++ show k

findSymbol :: O.ID -> CompileM O.Object
findSymbol sym = CompileM $ asks (lookup' sym . envDict)

findBinding :: String -> CompileM (DrawM ())
findBinding sym = CompileM $ asks (lookup' sym . envBindings)

addBindings :: Bindings -> CompileM a -> CompileM a
addBindings bindings = CompileM . local addBinding . runCompileM
    where
    addBinding env = env { envBindings = envBindings env `Map.union` bindings }

loadTexture :: String -> CompileM GL.TextureObject
loadTexture s = CompileM $ liftIO . ($ s) =<< asks envLoader

compile :: (String -> IO (GL.TextureObject)) -> O.Model -> IO (IO ())
compile loader model = do
    scale . runDrawM <$> evalStateT (runReaderT (runCompileM (compileVisualScene (O.modelScene model))) initEnv) Map.empty
    where
    initEnv = CompileEnv { envDict = O.modelDict model, envBindings = Map.empty, envLoader = loader }

    scale action = GL.preservingMatrix $ do
        let s = O.modelScale model in GL.scale s s s
        action

compileArray :: O.ID -> CompileM (Ptr GL.GLfloat)
compileArray = cached go
    where
    go (O.OFloatArray xs) = liftIO $ Array.newArray xs
    go _ = error "Not an array"

floatSize = Storable.sizeOf (undefined :: GL.GLfloat)

compileAccessor :: O.Accessor -> CompileM (GL.VertexArrayDescriptor GL.GLfloat)
compileAccessor (O.Accessor arrayid components count stride offset) = do
    ptr <- compileArray arrayid
    let ptr' = Array.advancePtr ptr offset
    return $ GL.VertexArrayDescriptor (fromIntegral components) GL.Float byteStride ptr'
    where
    byteStride = fromIntegral $ Storable.sizeOf floatSize * (stride - components)


indexDescriptor :: GL.VertexArrayDescriptor GL.GLfloat -> Int -> Int -> IO GL.GLfloat
indexDescriptor (GL.VertexArrayDescriptor components _ stride ptr) = \ix off ->
    Storable.peekByteOff ptr (step * ix + floatSize * off)
    where
    step = fromIntegral stride + floatSize * fromIntegral components

data CompiledInput = CompiledInput { ciSetupArrays :: DrawM (), ciIndex :: Int -> DrawM () }

instance Monoid CompiledInput where
    mempty = CompiledInput (return ()) (const (return ()))
    mappend a b = CompiledInput { ciSetupArrays = ciSetupArrays a >> ciSetupArrays b
                                , ciIndex = \z -> ciIndex a z >> ciIndex b z 
                                }

compileInput :: O.Input -> CompileM CompiledInput
compileInput (O.Input _ semantic source) = do
    sym <- findSymbol source
    case sym of
        O.OVertices inputs -> mconcat <$> mapM compileInput inputs
        O.OSource acc -> do
            descriptor <- compileAccessor acc
            let sem = convertSem semantic
            let setupArrays = liftIO $ do
                    GL.arrayPointer sem GL.$= descriptor
                    GL.clientState sem GL.$= GL.Enabled
            let ind = indexDescriptor descriptor 
            let index = case sem of
                            GL.VertexArray -> \z -> liftIO $ GL.vertex =<< liftM3 GL.Vertex3 (ind z 0) (ind z 1) (ind z 2)
                            GL.NormalArray -> \z -> liftIO $ GL.normal =<< liftM3 GL.Normal3 (ind z 0) (ind z 1) (ind z 2)
                            GL.TextureCoordArray -> \z -> liftIO $ GL.texCoord =<< liftM2 GL.TexCoord2 (ind z 0) (ind z 1)
                            x -> error $ "Don't know how to index a " ++ show x
            return $ CompiledInput setupArrays index
            
                    
        x -> error $ "Input can't use " ++ show x ++ " as a source"
    where
    convertSem O.SemPosition = GL.VertexArray
    convertSem O.SemNormal   = GL.NormalArray
    convertSem O.SemTexCoord = GL.TextureCoordArray
    conversion sem = error $ "Unknown semantic: " ++ show sem

compilePrimitive :: O.Primitive -> CompileM (DrawM ())
compilePrimitive (O.PrimTriangles material inputs indices) = do
    mat <- findBinding material
    case inputs of
        [inp] -> do  -- fast single-input
            compiled <- compileInput inp 
            liftIO $ do
                let numindices = fromIntegral (length indices)
                ixarray :: Ptr GL.GLuint <- liftIO . Array.newArray $ map fromIntegral indices
                return . wrapDrawM (GL.preservingClientAttrib [GL.AllClientAttributes]) $ do
                    mat
                    ciSetupArrays compiled
                    liftIO $ GL.drawElements GL.Triangles numindices GL.UnsignedInt ixarray
        
        inps -> do   -- slow multi-input  (should copy to a new vertex array instead)
            compiled <- Map.fromListWith mappend <$> mapM (\inp@(O.Input i _ _) -> ((,) i) <$> compileInput inp) inps
            let (maxE,example) = Map.findMax compiled
            return . (mat >>) . wrapDrawM (GL.renderPrimitive GL.Triangles) . forM_ (zip (cycle [0..maxE]) indices) $ \(inpix,ix) -> do
                ciIndex (Map.findWithDefault mempty inpix compiled) ix
            

compileGeometry :: O.ID -> CompileM (DrawM ())
compileGeometry = cached go
    where
    go (O.OGeometry (O.Mesh prims)) = do
        cprims <- mapM compilePrimitive prims
        return $ sequence_ cprims
    go _ = error "Not a geometry"

compileNode :: O.Node -> CompileM (DrawM ())
compileNode (O.Node (O.Matrix matrix) instances) = do
    mat :: GL.GLmatrix GL.GLfloat <- liftIO $ GL.newMatrix GL.RowMajor matrix 
    instances' <- mapM compileNodeInstance instances
    return . wrapDrawM GL.preservingMatrix $ liftIO (GL.multMatrix mat) >> sequence_ instances'

compileNodeInstance :: O.NodeInstance -> CompileM (DrawM ())
compileNodeInstance (O.NINode ref) = compileNodeRef ref
compileNodeInstance (O.NIGeometry geom materials) = do
    materials' <- Map.unions <$> mapM compileMaterialBinding materials
    addBindings materials' $ compileGeometry geom

compileNodeRef :: O.NodeRef -> CompileM (DrawM ())
compileNodeRef (O.NRNode node) = compileNode node
compileNodeRef (O.NRInstance nodeid) = compileNodeObject nodeid

compileNodeObject :: O.ID -> CompileM (DrawM ())
compileNodeObject = cached go
    where
    go (O.ONode node) = compileNode node
    go _ = error "Not a node"

compileVisualScene :: O.ID -> CompileM (DrawM ())
compileVisualScene = cached go
    where
    go (O.OVisualScene noderefs) = do
        sequence_ <$> mapM compileNodeRef noderefs
    go _ = error "Not a visual scene"

compileImage :: O.ID -> CompileM GL.TextureObject
compileImage = cached go
    where
    go (O.OImage path) = loadTexture path
    go _ = error "not an image"

compileEffect :: O.ID -> CompileM (DrawM ())
compileEffect = cached go
    where
    go (O.OEffect (O.TechLambert cot)) = lambert cot
    go (O.OEffect (O.TechConstant cot alpha)) = go (O.OEffect (O.TechLambert cot)) -- XXX Hax
    go (O.OMaterial ident) = compileEffect ident
    go x = error $ "Not an effect: " ++ show x
    
    lambert (O.COTColor r g b a) = return . liftIO $ do
        GL.texture GL.Texture2D GL.$= GL.Disabled
        GL.colorMaterial GL.$= Just (GL.Front, GL.Diffuse)
        GL.color $ GL.Color4 r g b a
    lambert (O.COTTexture source _texcoord) = do
        texobj <- compileParameter source
        return . liftIO $ do
            GL.colorMaterial GL.$= Just (GL.Front, GL.Diffuse)
            GL.texture GL.Texture2D GL.$= GL.Enabled
            GL.textureBinding GL.Texture2D GL.$= Just texobj

compileParameter :: O.ID -> CompileM GL.TextureObject
compileParameter = cached go
    where
    go (O.OParam (O.ParamSurface2D img)) = compileImage img
    go (O.OParam (O.ParamSampler2D surf)) = compileParameter surf
    go _ = error "Not a param"

compileMaterialBinding :: O.MaterialBinding -> CompileM Bindings
compileMaterialBinding (O.MaterialBinding sym target _ _) = do
    targetE <- compileEffect target
    return $ Map.singleton sym targetE
