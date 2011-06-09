-- author: tm8st (tm8st@hotmail.co.jp)
-- define scene data in HGI.

module Scene where

import Math

-- | Material, define object physical properties.
data Material = Material { mtDiffuseColor :: Vector3
                         , mtSpecularPower :: Double
                         }
                deriving(Eq, Show)

-- | Light.
data Light = PointLight{ plRadius :: Double
                       , plLocation :: Vector3
                       , plColor :: Vector3 
                       }
             deriving (Eq, Show)

-- | Camera.
data Camera = Camera { camLocation :: Vector3
                     }
              deriving(Eq, Show)

-- | wrap math object and mesh.
data Object = Object Sphere Material
            deriving(Eq, Show)

-- | define situation for render.
data Scene = Scene { objects :: [Object]
                   , lights :: [Light]
                   , camera :: Camera
                   , backgroundColor :: Color
                   }
             deriving(Eq, Show)
