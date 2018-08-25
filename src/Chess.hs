{-# LANGUAGE ScopedTypeVariables #-}
module Chess
  (
    Color(..),
    Position(..),
    Piece(..),
    PieceInstance(..),
    Board,
    legalMoves,
    positionExtractor,
    consPosM
  ) where

import Data.Maybe
import Data.Tuple
import Data.List

import DChains

--Generic machinery
allP :: [(a -> Bool)] -> (a -> Bool)
allP ps = foldl (\state pred -> (\x -> pred x && state x))  (\_ -> True) ps

anyP :: [(a -> Bool)] -> (a -> Bool)
anyP ps = foldl (\state pred -> (\x -> pred x || state x)) (\_ -> False) ps

headOption :: [a] -> Maybe a
headOption [] = Nothing
headOption (x : xs) = Just x

--Borrowed from https://www.stackage.org/haddock/lts-12.7/syb-0.7/Data-Generics-Aliases.html
orElse :: Maybe a -> Maybe a -> Maybe a
orElse x y = case x of
                 Just _  -> x
                 Nothing -> y

optReplaceWith :: (a -> a -> Bool) -> [a] -> a -> a
-- find predicate in xs return x if found else y, remove Maybe container which is
-- safe because y is not assumed to be Nothing.
optReplaceWith dp xs y = fromJust (orElse (find (\x -> dp y x) xs) $ Just y)

--End generic machinery

--Color
data Color = Black | White deriving (Show, Eq)

cflip :: Color -> Color
cflip Black = White
cflip White = Black
--End Color

--Piece
data Piece = Rook | Bisshop | King | Queen | Knight | Pawn deriving (Show, Eq)
--End Piece

--Position and constructors
data Position = Position Int Int deriving (Show, Eq, Ord)
constructablePos :: Int -> Int -> Bool
constructablePos x y = 0 < x && x <= 8 && 0 < y && y <= 8

consPosM :: Int -> Int -> Maybe Position
consPosM x y | (constructablePos x y) = Just $ Position x y
             | otherwise = Data.Maybe.Nothing

consPos :: Int -> Int -> Position
consPos x y | (constructablePos x y) = Position x y
            | otherwise = error "Out of reach"

tupleToPosM :: (Int, Int) -> Maybe Position
tupleToPosM (x,y) = consPosM x y

humanReadablePosition :: Position -> (Char, Int)
humanReadablePosition (Position x y) = let letters = ['A'..'H']
                                           letter = letters !! (x - 1)
                                        in (letter, y)
--End position and constructors

--Pieceinstance + basic getters
data PieceInstance = PieceInstance Color Piece Position deriving (Show, Eq)
positionExtractor :: PieceInstance -> Position
positionExtractor (PieceInstance _ _ p) = p

colorExtractor :: PieceInstance -> Color
colorExtractor (PieceInstance c _ _) = c

pieceExtractor :: PieceInstance -> Piece
pieceExtractor (PieceInstance _ p _) = p
--End Pieceinstance + basic getters


--Board
type Board = [PieceInstance]

--If at some point we change the Board structure to be more complicated, this will help us greatly.
board2PieceInstances :: Board -> [PieceInstance]
board2PieceInstances b = b

--If at some point we change the Board structure to be more complicated, this will help us greatly.
pieceInstances2Board :: [PieceInstance] -> Board
pieceInstances2Board ps = ps
--End board




type PosPiece = (Position, PieceInstance)

board2pospiece :: Board -> [PosPiece]
board2pospiece board = map (\p -> (positionExtractor p, p)) $ board2PieceInstances board --TODO: use tupelize

collisions :: [Position] -> Board -> [PosPiece]
collisions reach board = filter (\t -> fst t `elem` reach) $ board2pospiece $ board2PieceInstances board


--Pieceinstance filters
posPieceFilter :: (PieceInstance -> Bool) -> [PosPiece] -> [PosPiece]
posPieceFilter f list = filter (\(_, piece) -> f piece) list

positionsFilter :: [Position] -> PieceInstance -> Bool
positionsFilter ps pi = positionExtractor pi `elem` ps

colorFilter :: Color -> PieceInstance -> Bool
colorFilter c pi = colorExtractor pi == c

pieceFilter :: Piece -> PieceInstance -> Bool
pieceFilter p pi = pieceExtractor pi == p

kingFilter = pieceFilter King
--End PieceInstancefilters

data Closer = Closer (PieceInstance -> Bool)
data SemiCloser = SemiCloser (PieceInstance -> Bool)
posPiece2D :: SemiCloser -> Closer -> PosPiece -> D Position
posPiece2D (SemiCloser s) (Closer c) (pos, pi) = if (c pi) then Closed pos else if (s pi) then SemiClosed pos else Open pos




--generate positions, get all existing, lift them to (Open Positions),
--map 'optReplaceWith' over them to replace with their respective judgement if it exists
--effectuateDChain (cutting list short according to predefined filters)
getRange :: [D Position] -> [Maybe Position] -> [D Position]
getRange judgement possiblePositions = effectuateDChain $ map (optReplaceWith dEq judgement) $ liftD $ catMaybes $ possiblePositions

legalMoves :: PieceInstance -> Board -> [Position]
--Most optimal amount of moves for a knight: 8
legalMoves (PieceInstance color Knight (Position x y)) board =
  let hRange = map (\t -> [(fst t, snd t + 1), (fst t, snd t - 1)]) [(x + 2, y), (x - 2, y)]
      vRange = map (\t -> [(fst t + 1, snd t), (fst t - 1, snd t)]) [(x, y + 2), (x, y - 2)]
      reach = catMaybes $ map tupleToPosM $ concat $ hRange ++ vRange
      allCollisions :: [PosPiece] --Get all collisions in the theoretic reach
      allCollisions = collisions reach $ board2PieceInstances board
      selfOccupied = map fst $ posPieceFilter (colorFilter color) allCollisions
      --One can't take the enemyKing
      enemyKing = map fst $ posPieceFilter (allP [(colorFilter $ cflip color), kingFilter]) allCollisions
      moves = filter (\x ->  not $ x `elem` (enemyKing ++ selfOccupied)) reach
  in (moves)

legalMoves (PieceInstance color King (Position x y)) board =
  let semiClose = SemiCloser $ colorFilter $ cflip color
      close = Closer $ anyP [colorFilter color, kingFilter]
      collisionJudgement = map (posPiece2D semiClose close) $ board2pospiece board
      vrangeF  = getRange collisionJudgement $ return (consPosM x (y + 1))
      vrangeB  = getRange collisionJudgement $ return (consPosM x (y -1))
      hrangeF  = getRange collisionJudgement $ return (flip consPosM y (x + 1))
      hrangeB  = getRange collisionJudgement $ return (flip consPosM y (x - 1))
      drangeLF = getRange collisionJudgement $ return $ tupleToPosM (x + 1, y + 1)
      drangeLB = getRange collisionJudgement $ return $ tupleToPosM (x + 1, y - 1)
      drangeRF = getRange collisionJudgement $ return $ tupleToPosM (x - 1, y + 1)
      drangeRB = getRange collisionJudgement $ return $ tupleToPosM (x - 1, y - 1)
  in (unliftD $ vrangeF ++ vrangeB ++ hrangeF ++ hrangeB ++ drangeLF ++ drangeLB ++ drangeRF ++ drangeRB)

legalMoves (PieceInstance color Bisshop (Position x y)) board =
  let semiClose = SemiCloser $ colorFilter $ cflip color
      close = Closer $ anyP [colorFilter color, kingFilter]
      collisionJudgement = map (posPiece2D semiClose close) $ board2pospiece board
      fLeft =   getRange collisionJudgement $ map (\n -> tupleToPosM $ (x + n, y + n)) [1..7]
      bLeft =   getRange collisionJudgement $ map (\n -> tupleToPosM $ (x + n, y - n)) [1..7]
      fRight =  getRange collisionJudgement $ map (\n -> tupleToPosM $ (x - n, y + n)) [1..7]
      bRight =  getRange collisionJudgement $ map (\n -> tupleToPosM $ (x - n, y - n)) [1..7]
  in (unliftD $ fLeft ++ bLeft ++ fRight ++ bRight)

legalMoves (PieceInstance color Queen (Position x y)) board =
  let semiClose = SemiCloser $ colorFilter $ cflip color
      close = Closer $ anyP [colorFilter color, kingFilter]
      collisionJudgement = map (posPiece2D semiClose close) $ board2pospiece board
      vrangeF  = getRange collisionJudgement $ map (consPosM x) $ map ((+) y) [1..7]
      vrangeB  = getRange collisionJudgement $ map (consPosM x) $ map ((-) y) [1..7]
      hrangeF  = getRange collisionJudgement $ map (flip consPosM y) $ map ((+) x) [1..7]
      hrangeB  = getRange collisionJudgement $ map (flip consPosM y) $ map ((-) x) [1..7]
      drangeLF = getRange collisionJudgement $ map (\n -> tupleToPosM $ (x + n, y + n)) [1..7]
      drangeLB = getRange collisionJudgement $ map (\n -> tupleToPosM $ (x + n, y - n)) [1..7]
      drangeRF = getRange collisionJudgement $ map (\n -> tupleToPosM $ (x - n, y + n)) [1..7]
      drangeRB = getRange collisionJudgement $ map (\n -> tupleToPosM $ (x - n, y - n)) [1..7]
  in (unliftD $ vrangeF ++ vrangeB ++ hrangeF ++ hrangeB ++ drangeLF ++ drangeLB ++ drangeRF ++ drangeRB)

legalMoves (PieceInstance color Rook (Position x y)) board =
  let semiClose = SemiCloser $ colorFilter $ cflip color
      close = Closer $ anyP [colorFilter color, kingFilter]
      collisionJudgement = map (posPiece2D semiClose close) $ board2pospiece board
      vrange =  getRange collisionJudgement $ map (consPosM x) $ map ((+) y) [1..7]
      vrange2 = getRange collisionJudgement $ map (consPosM x) $ map ((-) y) [1..7]
      hrange =  getRange collisionJudgement $ map (flip consPosM y) $ map ((+) x) [1..7]
      hrange2 = getRange collisionJudgement $ map (flip consPosM y) $ map ((-) x) [1..7]
  in (unliftD $ vrange ++ vrange2 ++ hrange ++ hrange2)

legalMoves (PieceInstance White Pawn (Position x y)) board = --y should be lower than 2, represent that?
  let range = if y == 2 then [1..2] else [1]
      direction = (+)
      moves = pawnMovement range x y White direction board
  in (moves)

legalMoves (PieceInstance Black Pawn (Position x y)) board = --y should be higher than 6, represent that?
  let range = if y == 6 then [1..2] else [1]
      direction = (-)
      moves = pawnMovement range x y Black direction board
  in (moves)

pawnMovement :: [Int] -> Int -> Int -> Color -> (Int -> Int -> Int) -> Board -> [Position]
pawnMovement range x y color direction board =
  let takeablePositions = catMaybes $ map tupleToPosM [(x+1, direction y 1), (x-1, direction y 1)]
      closer  = Closer $ anyP [colorFilter color, kingFilter]
      semiCloser = SemiCloser  $ allP [colorFilter $ cflip color, positionsFilter takeablePositions]
      collisionJudgement = map (posPiece2D semiCloser closer) $ board2pospiece board
      forward =    getRange collisionJudgement $ map (consPosM x) $ map (direction y) range
      takeLeft =   getRange collisionJudgement $ return $ headOption takeablePositions
      takeRight =  getRange collisionJudgement $ return $ headOption $ drop 1 takeablePositions
  in (unliftD $ forward  ++ takeRight ++ takeLeft)

