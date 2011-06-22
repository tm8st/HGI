-- author: tm8st (tm8st@hotmail.co.jp)
-- Global illumination renderer implement in haskell.

module Mesh where

import Text.Parsec
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle)
import Data.Char
import Math as M
import OBJ as O

-- texel data format.
data Texel = Texel { u, v :: Double
                   }
             deriving (Eq, Show)

-- vertex data format.
data Vertex = Vertex { vertPosition :: M.Vector3
                     , vertNormal :: M.Vector3
                     -- , texel :: Texel
                     }
              deriving (Eq, Show)

-- data Triangle = Triangle { v0, v1, v2, n :: Vector3
--                          }

-- mesh data format.
data Mesh = Mesh { vertexs :: [Vertex]
                 , triangles :: [Triangle]
                 }
          deriving (Eq, Show)

objMeshFromFileContent :: String -> Either ParseError Mesh
objMeshFromFileContent objtxt =
  case parseOBJ objtxt of
      Right objcontents -> Right (meshFromOBJFileContents $ contentsToInfo objcontents)
      Left reason -> Left reason

meshFromOBJFileContents :: ObjFileInfo -> Mesh
meshFromOBJFileContents objinfo = Mesh vs tris
  where
    vs = map (\(ObjVertex (O.Vector3 x y z) _,
                ObjNormal (O.Vector3 nx ny nz)) -> Vertex (M.Vector3 x y z) (M.Vector3 nx ny nz))
             (zip (positions objinfo) (normals objinfo))
    tris = foldl facesToTri [] $ faces objinfo
    facesToTri :: [Triangle] -> ObjFileContent -> [Triangle]
    facesToTri acc (ObjFace (a:b:c:[])) = acc ++ [triangleFromPoints (vertexIdxToVertex a) (vertexIdxToVertex c) (vertexIdxToVertex b)]
    facesToTri acc (ObjFace (a:b:c:d:[])) = acc ++ [ triangleFromPoints (vertexIdxToVertex a) (vertexIdxToVertex c) (vertexIdxToVertex b)
                                                , triangleFromPoints (vertexIdxToVertex c) (vertexIdxToVertex d) (vertexIdxToVertex a)]
    
    vertexIdxToVertex :: FaceVertex -> M.Vector3
    vertexIdxToVertex (FaceVertex v vt vn) = objv v
    objv i = let ObjVertex (O.Vector3 x y z) _ = (positions objinfo) !! i
             in M.Vector3 (x*5) (y*5) (z*5)
             -- in M.Vector3 x y z
    objvn i = let ObjNormal n = (normals objinfo) !! i
             in n

