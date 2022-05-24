import Data.Maybe

-- Maybe
safediv :: Integral a => a -> a -> Maybe a
safediv a b =
    if b == 0 then Nothing else Just $ div a b

a = isJust (safediv 0 1)
b = isNothing (safediv 0 0)
c = fromMaybe 0 (safediv 0 0)
