-- author: tm8st (tm8st@hotmail.co.jp)
-- Image Filters.

import Math

-- | Image type for filter.
data Image = Image { imgWidth :: Int
                   , imgHeight :: Int
                   , imgData :: [Color]
                   }
             deriving (Eq)

-- | Filter function type.
type Filter = Image -> Image

-- morphologicalAA :: Filter

-- | monotone filter.
monotone :: Filter
monotone img = img{ imgData = map f (imgData img) }
  where
    f c = let p = min 1.0 $ csize c
          in Color p p p

-- | highContrast filter.
highContrast :: Filter
highContrast img = img{ imgData = map f (imgData img) }
  where
    f c = let p = min 1.0 $ csize c
              p' = p*p
          in c `cmulByScalar` p'

-- | negaposi filter.
negaposi :: Filter
negaposi img = img{ imgData = map f (imgData img) }
  where
    f c = negate c
