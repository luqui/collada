module Graphics.Formats.Collada.Objects
    ( Dict, ID
    , Object(..), Matrix(..)
    , Accessor(..), Input(..), InputSemantic(..), Primitive(..)
    , Mesh(..), Parameter(..), Technique(..), LambertTechnique(..)
    , ColorOrTexture(..), Node(..), NodeRef(..), NodeInstance(..)
    , MaterialBinding(..), parseCollada
    )
where

import Prelude hiding ((.), id)
import qualified Text.XML.HXT.Arrow as X
import qualified Text.XML.HXT.Arrow.ParserInterface as X
import qualified Control.Arrow.ListArrow as LA
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Control.Category
import Control.Arrow
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.List

type Dict = Map.Map ID Object
type ID = String

data Object
    = OVisualScene [NodeRef]
    | OFloatArray [GL.GLfloat]
    | OSource Accessor
    | OVertices [Input]
    | OGeometry Mesh
    | OImage FilePath
    | OParam Parameter
    | OEffect Technique
    | OMaterial ID -- instance effect
    | ONode Node
    deriving Show

data Matrix
    = Matrix [GL.GLfloat]
    deriving Show

identityMatrix :: Matrix
identityMatrix = Matrix [ 1, 0, 0, 0
                        , 0, 1, 0, 0
                        , 0, 0, 1, 0
                        , 0, 0, 0, 1 ]

data Accessor
    = Accessor ID Int Int Int Int -- array components count stride offset
    deriving Show

data Input
    = Input Int InputSemantic ID -- offset semantic source
    deriving Show

data InputSemantic
    = SemPosition
    | SemNormal
    | SemVertex
    | SemTexCoord
    deriving (Eq,Show)

data Primitive
    = PrimTriangles String [Input] [Int]  -- material inputs indices
    deriving Show

data Mesh = Mesh [Primitive]
    deriving Show

data Parameter
    = ParamSurface2D ID
    | ParamSampler2D ID
    deriving Show

data Technique
    = TechLambert LambertTechnique
    deriving Show

data LambertTechnique
    = LambertTechnique ColorOrTexture -- diffuse
    deriving Show

data ColorOrTexture
    = COTColor GL.GLfloat GL.GLfloat GL.GLfloat GL.GLfloat
    | COTTexture ID String   -- source texcoord
    deriving Show

data Node
    = Node Matrix [NodeInstance]
    deriving Show

data NodeRef
    = NRNode Node
    | NRInstance ID
    deriving Show

data NodeInstance
    = NINode NodeRef
    | NIGeometry ID [MaterialBinding]
    deriving Show

data MaterialBinding
    = MaterialBinding String ID String String -- symbol target semantic input_semantic
    deriving Show

main = print . parseCollada =<< getContents

parseCollada :: String -> Maybe (ID, Dict)
parseCollada = listToMaybe . LA.runLA (mainA <<< X.parseXmlDoc <<^ (\x -> ("<stdin>", x)))

mainA :: LA.LA X.XmlTree (ID, Dict)
mainA = mainScene &&& (Map.unions .< X.multi objects) <<< X.hasName "COLLADA"

infixr 1 .<
(.<) = flip (X.>.)

refAttr :: String -> LA.LA X.XmlTree ID
refAttr name = stripHash ^<< X.getAttrValue0 name
    where
    stripHash ('#':x) = x
    stripHash x = x

objects = asum [ float_array, source, vertices, geometry, image, newparam, effect, material, node, visual_scene ]

mainScene :: LA.LA X.XmlTree ID
mainScene = refAttr "url" <<< child (X.hasName "instance_visual_scene") <<< child (X.hasName "scene")

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
accessor = massage ^<< (length .< child (X.hasName "param")) &&& refAttr "source" &&& X.getAttrValue0 "count" &&& X.getAttrValue "stride" &&& X.getAttrValue "offset" <<< X.hasName "accessor"
    where
    massage (len, (source, (count, (stride, offset)))) = Accessor source len (read count) (readDef len stride) (readDef 0 offset)

readDef d "" = d
readDef _ s  = read s

child n = n <<< X.getChildren

source :: LA.LA X.XmlTree Dict
source = object "source" $ OSource ^<< accessor <<< X.getChildren <<< child (X.hasName "technique_common")

input :: LA.LA X.XmlTree Input
input = massage ^<< X.getAttrValue "offset" &&& X.getAttrValue0 "semantic" &&& refAttr "source" <<< X.hasName "input"
    where
    massage (offset, (semantic, source)) = Input (readDef (-1) offset) (massageSemantic semantic) source -- -1 hax!!  See vertices where this is fixedup.
    massageSemantic "POSITION" = SemPosition
    massageSemantic "NORMAL"   = SemNormal
    massageSemantic "VERTEX"   = SemVertex
    massageSemantic "TEXCOORD" = SemTexCoord
    massageSemantic s = error $ "Unknown semantic: " ++ s

vertices :: LA.LA X.XmlTree Dict
vertices = object "vertices" $ OVertices . fixups .< child input
    where
    fixups = zipWith fixup [0..]
    fixup n (Input z sem source) | z == -1 = Input n sem source
                                 | otherwise = Input z sem source


triangles :: LA.LA X.XmlTree Primitive
triangles = massage ^<< X.getAttrValue "material" &&& procBody <<< X.hasName "triangles"
    where
    procBody = (id .< child input) &&& (map read . words ^<< child X.getText <<< child (X.hasName "p"))
    massage (material, (inputs, p)) = PrimTriangles material inputs p

mesh :: LA.LA X.XmlTree Mesh
mesh = (Mesh .< child primitives) <<< X.hasName "mesh"
    where
    primitives = asum [ triangles ]

geometry :: LA.LA X.XmlTree Dict
geometry = object "geometry" $ OGeometry ^<< child mesh

image :: LA.LA X.XmlTree Dict
image = object "image" $ OImage ^<< child X.getText <<< child (X.hasName "init_from")

newparam :: LA.LA X.XmlTree Dict
newparam = objectWithIDAttr "sid" "newparam" $ OParam ^<< asum [surface, sampler2D] <<< X.getChildren
    where
    surface = ParamSurface2D ^<< child X.getText <<< child (X.hasName "init_from") <<< X.hasAttrValue "type" (== "2D") <<< X.hasName "surface"
    sampler2D = ParamSampler2D ^<< child X.getText <<< child (X.hasName "source") <<< X.hasName "sampler2D"

colorOrTexture :: LA.LA X.XmlTree ColorOrTexture
colorOrTexture = texture X.<+> color
    where
    texture = uncurry COTTexture ^<< X.getAttrValue0 "texture" &&& X.getAttrValue0 "texcoord" <<< X.hasName "texture"
    color = colorify . map read . words ^<< child X.getText <<< X.hasName "color"
    colorify [r,g,b,a] = COTColor r g b a
    colorify s = error "Malformed color"

lambert :: LA.LA X.XmlTree LambertTechnique
lambert = LambertTechnique ^<< child colorOrTexture <<< child (X.hasName "diffuse") <<< X.hasName "lambert"

technique :: LA.LA X.XmlTree Technique
technique = asum [TechLambert ^<< lambert] <<< X.getChildren <<< X.hasName "technique"

effect :: LA.LA X.XmlTree Dict
effect = object "effect" $ OEffect ^<< child technique <<< child (X.hasName "profile_COMMON")

material :: LA.LA X.XmlTree Dict
material = object "material" $ OMaterial ^<< refAttr "url" <<< child (X.hasName "instance_effect")

nodeRef :: LA.LA X.XmlTree NodeRef
nodeRef = asum [inline, instance_node] 
    where
    inline = (arr NRInstance ||| (NRNode ^<< rawNode)) <<< switch <<< X.hasName "node"
    switch = convid ^<< X.getAttrValue "id" &&& id
    convid ("", xml) = Right xml
    convid (x, _)    = Left x

instance_node :: LA.LA X.XmlTree NodeRef
instance_node = NRInstance ^<< refAttr "url" <<< X.hasName "instance_node"

nodeInstance :: LA.LA X.XmlTree NodeInstance
nodeInstance = asum [NINode ^<< nodeRef, instance_geometry]

instance_geometry :: LA.LA X.XmlTree NodeInstance
instance_geometry = uncurry NIGeometry ^<< refAttr "url" &&& bindings <<< X.hasName "instance_geometry"
    where
    bindings = id .< (child instance_material <<< child (X.hasName "technique_common") <<< child (X.hasName "bind_material"))

matrix :: LA.LA X.XmlTree Matrix
matrix = Matrix . map read . words ^<< child X.getText <<< X.hasName "matrix"

rawNode :: LA.LA X.XmlTree Node
rawNode = uncurry Node ^<< (child matrix `X.withDefault` identityMatrix) &&& (id .< child nodeInstance) <<< X.hasName "node"

node :: LA.LA X.XmlTree Dict
node = object "node" $ ONode ^<< rawNode

instance_material :: LA.LA X.XmlTree MaterialBinding
instance_material = conv ^<< myAttrs &&& bindAttrs <<< X.hasName "instance_material"
    where
    conv ((symbol, target), (semantic, input_semantic)) = MaterialBinding symbol target semantic input_semantic
    myAttrs = X.getAttrValue0 "symbol" &&& refAttr "target"
    bindAttrs = X.getAttrValue0 "semantic" &&& X.getAttrValue0 "input_semantic" <<< child (X.hasName "bind_vertex_input")

visual_scene :: LA.LA X.XmlTree Dict
visual_scene = object "visual_scene" $ OVisualScene ^<< id .< child nodeRef
