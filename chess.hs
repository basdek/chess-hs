module Chess
  (
    legalMoves,
    consPos,
    PieceInstance
  ) where

import Data.Maybe

data Color = Black | White deriving (Show, Eq)

data Piece = Rook | Bisshop | King | Queen | Knight | Pawn deriving (Show, Eq)

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
humanReadablePosition (Position x y) = let letters = ['A'..'H']
                                           letter = letters !! (x - 1)
                                        in (letter, y)

--Helper creating a diagonal position from an x, y coordinate.
diagonalAdder :: Int -> Int -> Int -> Maybe Position
diagonalAdder x y n  =  consPosM (x + n) (y + n)

fullRange = [(-8)..8]

data Board = Board

check :: Board -> Bool
check _ = False


legalMoves :: Piece -> Position -> [Position]

legalMoves Rook pos = let x = h pos
                          y = v pos
                          vRange = map (consPosM x) $ map (\n -> n + y) fullRange
                          hRange = map (flip consPosM y) $ map (\n -> n + x) fullRange

                          --Get only on the grid moves, filter out the starting position.
                          moves = filter (\a -> a /= pos) $ catMaybes $ hRange ++ vRange
                      in moves

legalMoves Queen pos = let  x = h pos
                            y = v pos
                            hRange = map (consPosM x) $ map (\n -> n + y) fullRange
                            vRange = map (flip consPosM y) $ map (\n -> n + x) fullRange
                            dRange = map (diagonalAdder x y) fullRange

                            --Get only on the grid moves (horizontal, vertical, diagonal),
                            --filter out starting position.
                            moves = filter (\a -> a /= pos) $ catMaybes $ vRange ++ hRange ++ dRange
                       in moves

legalMoves Bisshop pos = let x = h pos
                             y = v pos
                             --Get only on the grid moves, filter out the starting position.
                             moves = filter (\a -> a /= pos) $ catMaybes $ map (diagonalAdder x y) fullRange
                         in moves

legalMoves King pos = let x = h pos
                          y = v pos
                          range1 = [(-1)..1]
                          hRange = map (consPosM x) $ map (\n -> n + y) range1
                          vRange = map (flip consPosM y) $ map (\n -> n + x) range1
                          dRange = map (diagonalAdder x y) range1
                          --Get only on the grid moves, filter out the starting position.
                          moves = filter (\a -> a /= pos) $ catMaybes $ hRange ++ vRange ++ dRange
                       in moves

--Handling a start position for white.
legalMoves Pawn (Position x 2) = [Position x 3, Position x 4]
--Handling a start position for black.
legalMoves Pawn (Position x 7) = [Position x 6, Position x 5]
--Handling all non starting positions.
legalMoves Pawn (Position x y) = [Position x (y+1)]
