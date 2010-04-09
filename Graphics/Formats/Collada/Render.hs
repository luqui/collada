{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Graphics.Formats.Collada.Render where

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Formats.Collada.Objects as O
import qualified Data.Map as Map
import qualified Foreign.Marshal.Array as Array
import Foreign.Ptr (Ptr)
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans

newtype DrawM a = DrawM (IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

preservingMatrix :: DrawM a -> DrawM a
preservingMatrix (DrawM io) = DrawM (GL.preservingMatrix io)

newtype CompileM a = CompileM (ReaderT O.Dict IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

findSymbol :: O.ID -> CompileM O.Object
findSymbol ident = CompileM $ asks (Map.! ident)


compileArray :: O.Object -> CompileM (Ptr GL.GLfloat)
compileArray (O.OFloatArray xs) = liftIO $ Array.newArray xs
compileArray _ = error "Not an array"

compileSource :: O.Object -> CompileM (GL.VertexArrayDescriptor GL.GLfloat)
compileSource (O.OSource (O.Accessor arrayid count stride offset)) = do
    ptr <- compileArray =<< findSymbol arrayid
    let ptr' = Array.advancePtr ptr offset
    -- XXX stride is wrong for NumComponents!
    return $ GL.VertexArrayDescriptor (fromIntegral stride) GL.Float (fromIntegral stride) ptr'
compileSource _ = error "Not a source"

compileInput :: O.Input -> CompileM (DrawM ())
compileInput (O.Input _ semantic source) = do
    descriptor <- compileSource =<< findSymbol source
    let sem = convertSem semantic
    return . liftIO $ do
        GL.arrayPointer sem GL.$= descriptor
        GL.clientState sem GL.$= GL.Enabled
    where
    convertSem O.SemPosition = GL.VertexArray
    convertSem O.SemNormal   = GL.NormalArray
    convertSem O.SemVertex   = GL.VertexArray  -- hmmm
    convertSem O.SemTexCoord = GL.TextureCoordArray

compilePrimitive :: O.Primitive -> CompileM (DrawM ())
compilePrimitive (O.PrimTriangles _material inputs indices) = do
    case inputs of
        [inp] -> do
            inpinit <- compileInput inp 
            liftIO $ do
                let numindices = fromIntegral (length indices)
                ixarray :: Ptr GL.GLfloat <- liftIO . Array.newArray $ map fromIntegral indices
                return $ do
                    inpinit
                    liftIO $ GL.drawElements GL.Triangles numindices GL.Float ixarray
        
        _ -> return (return ())  -- only handling one input for now
                                 -- more requires copying because OpenGL is not cool enough

compileGeometry :: O.Object -> CompileM (DrawM ())
compileGeometry (O.OGeometry (O.Mesh prims)) = do
    cprims <- mapM compilePrimitive prims
    return $ sequence_ cprims

compileNode :: O.Node -> CompileM (DrawM ())
compileNode (O.Node (O.Matrix matrix) instances) = do
    mat :: GL.GLmatrix GL.GLfloat <- liftIO $ GL.newMatrix GL.RowMajor matrix 
    instances' <- mapM compileNodeInstance instances
    return . preservingMatrix $ liftIO (GL.multMatrix mat) >> sequence_ instances'

compileNodeInstance :: O.NodeInstance -> CompileM (DrawM ())
compileNodeInstance (O.NINode ref) = compileNodeRef ref
compileNodeInstance (O.NIGeometry geom _materials) = compileGeometry =<< findSymbol geom 

compileNodeRef :: O.NodeRef -> CompileM (DrawM ())
compileNodeRef (O.NRNode node) = compileNode node
compileNodeRef (O.NRInstance nodeid) = compileNodeObject =<< findSymbol nodeid

compileNodeObject :: O.Object -> CompileM (DrawM ())
compileNodeObject (O.ONode node) = compileNode node
compileNodeObject _ = error "Not a node"

compileVisualScene :: O.Object -> CompileM (DrawM ())
compileVisualScene (O.OVisualScene noderefs) = return . sequence_ =<< mapM compileNodeRef noderefs
