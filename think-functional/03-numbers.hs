-- Exercise A
-- Express subtract using flip.
subtract' = flip (-)

-- Exercise C
div' :: (Integral a) => a -> a -> a
div' x y = floor (fromIntegral x / fromIntegral y)

-- Exercise F
sqrt' :: Float -> Float
sqrt' x = until goodenough improve x
  where
    goodenough y = abs (y * y - x) < eps * x
    improve y = (y + x / y) / 2
    eps = 0.0000001
