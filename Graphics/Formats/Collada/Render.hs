{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards #-}

module Graphics.Formats.Collada.Render 
    ( compile )
where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Formats.Collada.Objects as O
import qualified Data.Map as Map
import qualified Foreign.Marshal.Array as Array
import Foreign.Ptr (Ptr)
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Control.Monad (when)

newtype DrawM a = DrawM { runDrawM :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

preservingMatrix :: DrawM a -> DrawM a
preservingMatrix (DrawM io) = DrawM (GL.preservingMatrix io)

preservingClientState :: DrawM a -> DrawM a
preservingClientState (DrawM io) = DrawM (GL.preservingClientAttrib [GL.AllClientAttributes] io)

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
compileArray arr@(O.OFloatArray xs) = debug arr >> (liftIO $ Array.newArray xs)
compileArray _ = error "Not an array"

debug x = liftIO $ putStrLn ("\nCompiling: " ++ show x)

compileAccessor :: O.Accessor -> CompileM (GL.VertexArrayDescriptor GL.GLfloat)
compileAccessor acc@(O.Accessor arrayid count stride offset) = do
    debug acc
    ptr <- compileArray =<< findSymbol arrayid
    let ptr' = Array.advancePtr ptr offset
    -- XXX Sooo wrong
    return $ GL.VertexArrayDescriptor 3 GL.Float 0 ptr'

compileInput :: O.Input -> CompileM (DrawM ())
compileInput inp@(O.Input _ semantic source) = do
    debug inp
    sym <- findSymbol source
    case sym of
        O.OVertices inputs -> sequence_ <$> mapM compileInput inputs
        O.OSource acc -> do
            descriptor <- compileAccessor acc
            let sem = convertSem semantic
            return . liftIO $ do
                GL.arrayPointer sem GL.$= descriptor
                GL.clientState sem GL.$= GL.Enabled
        x -> error $ "Input can't use " ++ show x ++ " as a source"
    where
    convertSem O.SemPosition = GL.VertexArray
    convertSem O.SemNormal   = GL.NormalArray
    convertSem O.SemVertex   = GL.VertexArray  -- hmmm
    convertSem O.SemTexCoord = GL.TextureCoordArray

compilePrimitive :: O.Primitive -> CompileM (DrawM ())
compilePrimitive prim@(O.PrimTriangles _material inputs indices) = do
    debug prim
    case inputs of
        [inp] -> do
            inpinit <- compileInput inp 
            liftIO $ do
                let numindices = fromIntegral (length indices)
                ixarray :: Ptr GL.GLuint <- liftIO . Array.newArray $ map fromIntegral indices
                return . preservingClientState $ do
                    inpinit
                    liftIO $ GL.drawElements GL.Triangles numindices GL.UnsignedInt ixarray
        
        _ -> return (return ())  -- only handling one input for now
                                 -- more requires copying because OpenGL is not cool enough

compileGeometry :: O.Object -> CompileM (DrawM ())
compileGeometry geom@(O.OGeometry (O.Mesh prims)) = do
    debug geom
    cprims <- mapM compilePrimitive prims
    return $ sequence_ cprims

compileNode :: O.Node -> CompileM (DrawM ())
compileNode node@(O.Node (O.Matrix matrix) instances) = do
    debug node
    mat :: GL.GLmatrix GL.GLfloat <- liftIO $ GL.newMatrix GL.RowMajor matrix 
    instances' <- mapM compileNodeInstance instances
    return . preservingMatrix $ liftIO (GL.multMatrix mat) >> sequence_ instances'

compileNodeInstance :: O.NodeInstance -> CompileM (DrawM ())
compileNodeInstance ni@(O.NINode ref) = debug ni >> compileNodeRef ref
compileNodeInstance ni@(O.NIGeometry geom _materials) = debug ni >> (compileGeometry =<< findSymbol geom)

compileNodeRef :: O.NodeRef -> CompileM (DrawM ())
compileNodeRef nr@(O.NRNode node) = debug nr >> compileNode node
compileNodeRef nr@(O.NRInstance nodeid) = debug nr >> (compileNodeObject =<< findSymbol nodeid)

compileNodeObject :: O.Object -> CompileM (DrawM ())
compileNodeObject (O.ONode node) = compileNode node
compileNodeObject _ = error "Not a node"

compileVisualScene :: O.Object -> CompileM (DrawM ())
compileVisualScene scene@(O.OVisualScene noderefs) = do
    debug scene
    sequence_ <$> mapM compileNodeRef noderefs
