{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards #-}

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
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Control.Monad (when, forM_, liftM2, liftM3)

newtype DrawM a = DrawM { runDrawM :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

wrapDrawM :: (IO a -> IO a) -> (DrawM a -> DrawM a)
wrapDrawM f = DrawM . f . runDrawM

newtype CompileM a = CompileM { runCompileM :: ReaderT O.Dict IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

lookup' :: (Ord k, Show k) => k -> Map.Map k a -> a
lookup' k mp | Just x <- Map.lookup k mp = x
             | otherwise                 = error $ "Couldn't find object: " ++ show k

findSymbol :: O.ID -> CompileM O.Object
findSymbol = CompileM . asks . lookup'

compile :: (O.ID, O.Dict) -> IO (IO ())
compile (mainid, dict) = runDrawM <$> runReaderT (runCompileM (compileVisualScene (lookup' mainid dict))) dict

compileArray :: O.Object -> CompileM (Ptr GL.GLfloat)
compileArray (O.OFloatArray xs) = (liftIO $ Array.newArray xs)
compileArray _ = error "Not an array"

floatSize = Storable.sizeOf (undefined :: GL.GLfloat)

compileAccessor :: O.Accessor -> CompileM (GL.VertexArrayDescriptor GL.GLfloat)
compileAccessor (O.Accessor arrayid components count stride offset) = do
    ptr <- compileArray =<< findSymbol arrayid
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
    convertSem O.SemVertex   = GL.VertexArray  -- hmmm
    convertSem O.SemTexCoord = GL.TextureCoordArray

compilePrimitive :: O.Primitive -> CompileM (DrawM ())
compilePrimitive (O.PrimTriangles _material inputs indices) = do
    case inputs of
        [inp] -> do  -- fast single-input
            compiled <- compileInput inp 
            liftIO $ do
                let numindices = fromIntegral (length indices)
                ixarray :: Ptr GL.GLuint <- liftIO . Array.newArray $ map fromIntegral indices
                return . wrapDrawM (GL.preservingClientAttrib [GL.AllClientAttributes]) $ do
                    ciSetupArrays compiled
                    liftIO $ GL.drawElements GL.Triangles numindices GL.UnsignedInt ixarray
        
        inps -> do   -- slow multi-input  (should copy to a new vertex array instead)
            compiled <- Map.fromListWith mappend <$> mapM (\inp@(O.Input i _ _) -> ((,) i) <$> compileInput inp) inps
            let (maxE,example) = Map.findMax compiled
            return . wrapDrawM (GL.renderPrimitive GL.Triangles) . forM_ (zip (cycle [0..maxE]) indices) $ \(inpix,ix) -> do
                let inp = Map.findWithDefault mempty inpix compiled
                ciIndex inp ix
        _ -> return (return ())
            
            

compileGeometry :: O.Object -> CompileM (DrawM ())
compileGeometry (O.OGeometry (O.Mesh prims)) = do
    cprims <- mapM compilePrimitive prims
    return $ sequence_ cprims

compileNode :: O.Node -> CompileM (DrawM ())
compileNode (O.Node (O.Matrix matrix) instances) = do
    mat :: GL.GLmatrix GL.GLfloat <- liftIO $ GL.newMatrix GL.RowMajor matrix 
    instances' <- mapM compileNodeInstance instances
    return . wrapDrawM GL.preservingMatrix $ liftIO (GL.multMatrix mat) >> sequence_ instances'

compileNodeInstance :: O.NodeInstance -> CompileM (DrawM ())
compileNodeInstance (O.NINode ref) = compileNodeRef ref
compileNodeInstance (O.NIGeometry geom _materials) = (compileGeometry =<< findSymbol geom)

compileNodeRef :: O.NodeRef -> CompileM (DrawM ())
compileNodeRef (O.NRNode node) = compileNode node
compileNodeRef (O.NRInstance nodeid) = (compileNodeObject =<< findSymbol nodeid)

compileNodeObject :: O.Object -> CompileM (DrawM ())
compileNodeObject (O.ONode node) = compileNode node
compileNodeObject _ = error "Not a node"

compileVisualScene :: O.Object -> CompileM (DrawM ())
compileVisualScene (O.OVisualScene noderefs) = do
    sequence_ <$> mapM compileNodeRef noderefs
