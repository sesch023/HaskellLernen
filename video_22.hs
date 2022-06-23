import Data.Either

data SomeData = Left Int | Right String

type SomeDataPoly = Either Int String
