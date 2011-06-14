-- author: tm8st (tm8st@hotmail.co.jp)
-- Image Filters.

data Image = Image { imgWidth :: Int
                   , imgHeight :: Int
                   , imgData :: [Color]
                   }

type Filter = Image -> Image

-- morphologicalAA :: Filter

monotone :: Filter
monotone img = img{ imgData = map f (imgData img) }
  where
    f c = let p = min 1.0 $ csize c
          in Color p p p

contrast :: Filter
contrast img = img{ imgData = map f (imgData img) }
  where
    f c = let p = min 1.0 $ csize c
              p' = p*p
          in c `cmulByScalar` p'

negaposi :: Filter
negaposi img = img{ imgData = map f (imgData img) }
  where
    f c = negate c
