module Graphics.Formats.Collada 
    ( Config(..), load, defaultConfig, pathTextureLoader )
where

import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Formats.Collada.Objects
import Graphics.Formats.Collada.Render
import qualified Codec.Image.STB as Image
import qualified Data.Bitmap.OpenGL as Bitmap

data Config = Config {
    textureLoader :: String -> IO GL.TextureObject
}

load :: Config -> String -> IO (IO ())
load config contents = do
    case parseCollada contents of
        Nothing -> fail "Parse error"
        Just model -> compile (textureLoader config) model

defaultConfig :: Config
defaultConfig = Config {
    textureLoader = pathTextureLoader "."
}

-- | Takes a prefix and returns a texture loader that prepends the prefix to the path
-- and loads from disk.
pathTextureLoader :: String -> String -> IO GL.TextureObject
pathTextureLoader prefix path = do
    let loc = prefix ++ "/" ++ path
    e <- Image.loadImage loc
    case e of
        Left err -> fail $ "Couldn't load " ++ loc ++ ": " ++ err
        Right bmp -> Bitmap.makeSimpleBitmapTexture bmp
