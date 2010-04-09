{-# LANGUAGE TypeFamilies #-}

import qualified Graphics.Formats.Collada.Render as Collada
import qualified Graphics.Formats.Collada.Objects as Collada
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Control.Applicative
import Data.VectorSpace

instance AdditiveGroup GL.GLdouble where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance VectorSpace GL.GLdouble where
    type Scalar GL.GLdouble = GL.GLdouble
    (*^) = (*)

instance InnerSpace GL.GLdouble where
    (<.>) = (*)

main = do
    GLUT.getArgsAndInitialize
    GLUT.createWindow "Hello!"

    Just model <- Collada.parseCollada <$> getContents
    action <- Collada.compile model

    GL.lighting GL.$= GL.Enabled
    GL.light (GL.Light 0) GL.$= GL.Enabled
    GL.position (GL.Light 0) GL.$= GL.Vertex4 0 0 0 1
    GL.depthFunc GL.$= Nothing
    GL.depthFunc GL.$= Just GL.Lequal
    GL.depthMask GL.$= GL.Enabled

    GLUT.displayCallback GLUT.$= (do
        GL.clearColor GL.$= GL.Color4 0.2 0 0 0
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.preservingMatrix $ do
            GL.matrixMode GL.$= GL.Projection
            GL.loadIdentity
            GLU.perspective 45 1 1 10000
            GL.matrixMode GL.$= GL.Modelview 0
            GL.loadIdentity
            GLU.lookAt (uncurry3 GL.Vertex3 eye) (uncurry3 GL.Vertex3 object) (uncurry3 GL.Vector3 up)
            action
        GLUT.swapBuffers)
    GLUT.mainLoop

    where
    eye = (0,200,120)
    object = (120, 0, 0)
    up = normalized $ (object ^-^ eye) `cross` ((object ^-^ eye) `cross` (0,1,0))
    cross (bx,by,bz) (cx,cy,cz) = (by*cz - cy*bz, bz*cx - bx*cz, bx*cy - by*cx)

    uncurry3 f (x,y,z) = f x y z
