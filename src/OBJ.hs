-- author: tm8st (tm8st@hotmail.co.jp)
-- read OBJ format file.

module OBJ where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Data.Either

-- | Vector3 in ObjFile.
data Vector3 = Vector3 Double Double Double
               deriving(Show, Eq)

-- | Obj file data types.
data ObjFileContent = ObjComment String
                    | ObjNormal Vector3
                    | ObjVertex Vector3 Vector3 -- ^ Location, Color
                    | ObjFace [FaceVertex]
                    deriving(Eq, Show)

-- | FaceVertex indecies (Vertex, Texel, Normal)
-- | converted 1 origin to 0 origin in parse.
data FaceVertex = FaceVertex Int Int Int
                  deriving(Eq, Show)

-- | Sign. used for only parse.
data Sign = Plus | Minus
          deriving(Eq, Show)

-- | Obj file data info.
data ObjFileInfo = ObjFileInfo { positions :: [ObjFileContent]
                               , normals :: [ObjFileContent]
                               , faces :: [ObjFileContent]
                               }
                   deriving(Eq, Show)

-- | .
contentsToInfo :: [ObjFileContent] -> ObjFileInfo
contentsToInfo objs = ObjFileInfo { positions = vs
                                  , normals = vns
                                  , faces = fs
                                  }
  where
    (vs, vns, fs, cs) = foldl ff ([], [], [], []) objs
    ff (a, b, c, d) o = case o of
             ObjVertex _ _ -> (a ++ [o], b, c, d)
             ObjNormal _ -> (a, b ++ [o], c, d)
             ObjFace _ -> (a, b, c ++ [o], d)
             ObjComment _ -> (a, b, c, d ++ [o])

-- | print readed objfile data.
printDetail :: ObjFileInfo -> IO ()
printDetail (ObjFileInfo vs vns fs) = 
  do
    putStrLn $ "vertecies " ++ (show $ length vs)
    putStrLn $ "normals " ++ (show $ length vns)
    putStrLn $ "faces " ++ (show $ length fs)

-- | parse OBJ file format string, very cheaper implement.
parseOBJ :: String -> Either ParseError [ObjFileContent]
parseOBJ input = parse objFile "(unknown state)" input

-- | root parser.
objFile :: Parser [ObjFileContent]
objFile = sepBy line (char '\n')

-- | parse line.
line :: Parser ObjFileContent
line =
  do
    try $ string "v "
    v <- vertex
    return v
  <|> do
    try $ string "vn "
    vn <- vertexNormal
    return vn
  <|> do
    string "f "
    f <- face
    return f
  <|> do
    string "#"
    c <- comment
    return c
  <|> do -- skip blank line.
    skipMany (noneOf ['\n'])
    return $ ObjComment "\n"

-- | parse vertex location and color.
vertex :: Parser ObjFileContent
vertex = 
  do
    -- string "v " already parsed in line parser.
    v1 <- vector3
    sp <- many (char ' ')
    color <- many vector3
    if null color
    then return $ ObjVertex v1 (head color)
    else return $ ObjVertex v1 (Vector3 1.0 1.0 1.0)
  <?> "vertex"

-- | parse vertex normal.
vertexNormal :: Parser ObjFileContent
vertexNormal = 
  do
    -- string "vn " already parsed in line parser.
    v <- vector3
    return $ ObjNormal v
  <?> "vertexNormal"

-- | parse face.
face :: Parser ObjFileContent
face = 
  do
    -- string "f " already parsed in line parser.
    fs <- sepBy faceVertex (char ' ')
    return $ ObjFace fs

-- | parse face vertex indecies and convert index origin.
faceVertex :: Parser FaceVertex
faceVertex = 
  do{
    v  <- number; char '/';
    vt <- many number; char '/';
    vn <- many number;

    let vt' = if null vt then -1 else (head vt) - 1
        vn' = if null vn then -1 else (head vn) - 1
    in return $ FaceVertex (v - 1) vt' vn'
    }

-- | parse comment.
comment :: Parser ObjFileContent
comment =
  do
    s <- many (noneOf ['\n'])
    return $ ObjComment s

-- | parse vertex.
vector3 :: Parser Vector3
vector3 =
  do
    x <- float; char ' '
    y <- float; char ' '
    z <- float;

    return (Vector3 x y z)
  <?> "vector3 error."

-- |
sign :: Parser Sign
sign = 
 do
   try $ char '-'
   return Minus
 <|>
   return Plus

-- |
float :: Parser Double
float =
  do
    s <- sign
    n1 <- number
    char '.'
    n2 <- number
    let f = (fromIntegral n1) + (pnt (fromIntegral n2))
    if s == Plus
    then return f
    else return $ negate f
  where
    pnt :: Double -> Double
    pnt n = if n < 1.0
            then n
            else pnt (n/10.0)

-- |
number :: Parser Int
number =
  do
    n <- many1 digit
    return $ read n
  <?> "number error"

{-| for test codes.
 -}
testLoadObj =
  do
    objtxt <- readFile "../resource/bunny-res4.obj"
    -- objtxt <- readFile "../resource/cube.obj"
    case parseOBJ objtxt of
      Right obj -> printDetail $ contentsToInfo obj
      Left reason -> putStrLn $ show reason

