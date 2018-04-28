module Chess
(
  legalMoves,
  consPos,
  PieceInstance
) where

import Data.Maybe

data Color = Black | White deriving (Show, Eq)

data Piece = Rook | Bisshop | King deriving (Show, Eq)

data PieceInstance = PieceInstance Color Piece deriving (Show, Eq)

data Position = Position Int Int deriving (Show, Eq, Ord)
v (Position _ y) = y
h (Position x _) = x


constructablePos :: Int -> Int -> Bool
constructablePos x y = 0 < x && x <= 8 && 0 < y && y <= 8

consPosM :: Int -> Int -> Maybe Position
consPosM x y | (constructablePos x y) = Just $ Position x y
             | otherwise = Nothing

consPos :: Int -> Int -> Position
consPos x y | (constructablePos x y) = Position x y
            | otherwise = error "Out of reach"

humanReadablePosition :: Position -> (Char, Int)
humanReadablePosition (Position x y) = let letters = ['a'..'h']
                                           letter = letters !! (x + 1)
                                       in (letter, y)


diagonalAdder :: Int -> Int -> Maybe Position
diagonalAdder x y = consPosM (x + 1) (y + 1)

diagonalSubtr :: Int -> Int -> Maybe Position
diagonalSubtr x y = consPosM (x - 1) (y - 1)


legalMoves :: Piece -> Position -> [Position]
legalMoves Rook pos = let x = h pos
                          y = v pos
                          range = [(-8)..8]
                          vRange = map (consPosM x) $ map (\n -> n + y) range
                          hRange = map (flip consPosM y) $ map (\n -> n + x) range

                          --Get only on the grid moves, filter out the starting position
                          moves = filter (\a -> a /= pos) $ catMaybes $ hRange ++ vRange
                      in moves

legalMoves King pos = let x = h pos
                          y = v pos
                          range = [(-1)..1]
                          hRange = map (consPosM x) $ map (\n -> n + y) range
                          vRange = map (consPosM y) $ map (\n -> n + x) range
                          moves = []
                      in moves

