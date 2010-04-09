import qualified Graphics.Formats.Collada.Render as Collada
import qualified Graphics.Formats.Collada.Objects as Collada
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Control.Applicative

main = do
    GLUT.getArgsAndInitialize
    GLUT.createWindow "Hello!"

    Just model <- Collada.parseCollada <$> getContents
    action <- Collada.compile model

    GL.lighting GL.$= GL.Enabled
    GL.light (GL.Light 0) GL.$= GL.Enabled
    GL.position (GL.Light 0) GL.$= GL.Vertex4 0 0 0 1

    GLUT.displayCallback GLUT.$= (do
        GL.clearColor GL.$= GL.Color4 0.2 0 0 0
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.preservingMatrix $ do
            GL.matrixMode GL.$= GL.Projection
            GL.loadIdentity
            GLU.perspective 45 1 1 10000
            GL.matrixMode GL.$= GL.Modelview 0
            GL.loadIdentity
            GLU.lookAt (GL.Vertex3 0 200 0) (GL.Vertex3 120 0 0) (GL.Vector3 1 0 0)
            action
        GLUT.swapBuffers)
    GLUT.mainLoop
