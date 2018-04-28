data Color = Black | White deriving (Show, Eq)

data Piece = Queen | King | Night deriving (Show, Eq)

data PieceInstance = PieceInstance {color :: Color, piece :: Piece} deriving(Show, Eq)



data Position = Int Int

v :: Position -> Int
v = undefined

h :: Position -> Int
h = undefined


humanReadablePosition :: Position -> (Char, Int)
humanReadablePosition = undefined


theoreticReach :: Piece -> Position -> [Position]
theoreticReach = undefined

