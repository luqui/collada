module Graphics.Formats.Collada where

import Prelude hiding ((.), id)
import qualified Text.XML.HXT.Arrow as X
import qualified Text.XML.HXT.Arrow.ParserInterface as X
import qualified Control.Arrow.ListArrow as LA
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Data.Map as Map
import Control.Category
import Control.Arrow
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.List

type Dict = Map.Map ID Object
type ID = String

data Object
    = OFloatArray [GL.GLfloat]
    | OSource Accessor
    | OVertices [Input]
    | OGeometry Mesh
    | OImage FilePath
    | OParam Parameter
    deriving Show

data Accessor
    = Accessor ID Int Int Int -- count stride offset
    deriving Show

data Input
    = Input Int InputSemantic ID -- offset semantic source
    deriving Show

data InputSemantic
    = SemPosition
    | SemNormal
    | SemVertex
    | SemTexCoord
    deriving Show

data Primitive
    = PrimTriangles String [Input] [Int]  -- material inputs indices
    deriving Show

data Mesh = Mesh [Primitive]
    deriving Show

data Parameter
    = ParamSurface2D ID
    | ParamSampler2D ID
    deriving Show

main = putStrLn . intercalate ("\n------------------\n") . map show . LA.runLA (mainA . X.parseXmlDoc) =<< fmap ((,) "<stdin>") getContents

--mainA :: LA.LA X.XmlTree Dict
mainA = (Map.unions .< X.multi objects) <<< X.hasName "COLLADA"

infixr 1 .<
(.<) = flip (X.>.)

objects = asum [ float_array, source, vertices, geometry, image, newparam ]

asum = foldr1 (X.<+>)

objectWithIDAttr :: String -> String -> LA.LA X.XmlTree Object -> LA.LA X.XmlTree Dict
objectWithIDAttr attr name proc = uncurry Map.singleton ^<< (X.getAttrValue0 attr &&& proc) . X.hasName name

object :: String -> LA.LA X.XmlTree Object -> LA.LA X.XmlTree Dict
object = objectWithIDAttr "id"

float_array :: LA.LA X.XmlTree Dict
float_array = object "float_array" $ toArray ^<< X.getText . X.getChildren
    where
    toArray = OFloatArray . map read . words

accessor :: LA.LA X.XmlTree Accessor
accessor = massage ^<< X.getAttrValue0 "source" &&& X.getAttrValue0 "count" &&& X.getAttrValue "stride" &&& X.getAttrValue "offset" <<< X.hasName "accessor"
    where
    massage (source, (count, (stride, offset))) = Accessor source (read count) (readDef 1 stride) (readDef 0 offset)

readDef d "" = d
readDef _ s  = read s

source :: LA.LA X.XmlTree Dict
source = object "source" $ OSource ^<< accessor <<< X.getChildren <<< X.hasName "technique_common" <<< X.getChildren 

input :: LA.LA X.XmlTree Input
input = massage ^<< X.getAttrValue "offset" &&& X.getAttrValue0 "semantic" &&& X.getAttrValue0 "source" <<< X.hasName "input"
    where
    massage (offset, (semantic, source)) = Input (readDef (-1) offset) (massageSemantic semantic) source -- -1 hax!!  See vertices where this is fixedup.
    massageSemantic "POSITION" = SemPosition
    massageSemantic "NORMAL"   = SemNormal
    massageSemantic "VERTEX"   = SemVertex
    massageSemantic "TEXCOORD" = SemTexCoord
    massageSemantic s = error $ "Unknown semantic: " ++ s

vertices :: LA.LA X.XmlTree Dict
vertices = object "vertices" $ OVertices . fixups .< (input <<< X.getChildren)
    where
    fixups = zipWith fixup [0..]
    fixup n (Input z sem source) | z == -1 = Input n sem source
                                 | otherwise = Input z sem source


triangles :: LA.LA X.XmlTree Primitive
triangles = massage ^<< X.getAttrValue "material" &&& procBody <<< X.hasName "triangles"
    where
    procBody = (id .< (input <<< X.getChildren)) &&& (map read . words ^<< X.getText <<< X.getChildren <<< X.hasName "p" <<< X.getChildren)
    massage (material, (inputs, p)) = PrimTriangles material inputs p

mesh :: LA.LA X.XmlTree Mesh
mesh = (Mesh .< (primitives <<< X.getChildren)) <<< X.hasName "mesh"
    where
    primitives = asum [ triangles ]

geometry :: LA.LA X.XmlTree Dict
geometry = object "geometry" $ OGeometry ^<< mesh <<< X.getChildren

image :: LA.LA X.XmlTree Dict
image = object "image" $ OImage ^<< X.getText <<< X.getChildren <<< X.hasName "init_from" <<< X.getChildren

newparam :: LA.LA X.XmlTree Dict
newparam = objectWithIDAttr "sid" "newparam" $ OParam ^<< asum [surface, sampler2D] <<< X.getChildren
    where
    surface = ParamSurface2D ^<< X.getText <<< X.getChildren <<< X.hasName "init_from" <<< X.hasAttrValue "type" (== "2D") <<< X.hasName "surface"
    sampler2D = ParamSampler2D ^<< X.getText <<< X.getChildren <<< X.hasName "source" <<< X.getChildren <<< X.hasName "sampler2D"
