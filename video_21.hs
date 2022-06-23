data Color = RGB (Int, Int, Int) 
    deriving (Show)

type Palette = [Color]

newtype Name = Name String