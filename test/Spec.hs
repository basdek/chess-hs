{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List
import Data.Maybe
import Control.Monad
import Test.HUnit.Base
import Test.Hspec
import Chess

tuplizeMap :: (a -> b) -> [a] -> [(a,b)]
tuplizeMap _ [] = []
tuplizeMap f (x:xs) = (x, f x) : tuplizeMap f xs

allPositions :: [Position]
allPositions = map (\(x,y) -> Position x y) [(h,v) | h <- [1..8], v <- [1..8]]

legalMovesF :: Board -> PieceInstance -> [Position]
legalMovesF = flip legalMoves

positionPlusX :: Position -> Int -> Maybe Position
positionPlusX (Position x y) n = consPosM (x+n) y

positionPlusY :: Position -> Int -> Maybe Position
positionPlusY (Position x y) n = flip consPosM (y+n) x

spec :: Spec
spec =   do
  describe "Chess.Spec allPositions" $ do
    it "should contain 64 positions" $ do
      length allPositions `shouldBe` 64
  describe "Chess.legalMoves King" $ do
    it "returns a list of 6 positions in the middle of an empty board." $
      let board = []
          pi = PieceInstance Black King $ Position 4 4
          moves = legalMoves pi board
      in (length moves `shouldBe` 8)
    it "returns a list of 3 positions for a corner position." $
      let board = []
          pi = PieceInstance Black King $ Position 1 1
          moves = legalMoves pi board
      in (length moves `shouldBe` 3)
    it "can not occupy a field that is occupied by a king." $
      let kingPos = Position 2 2
          board = [PieceInstance White King kingPos]
          pi = PieceInstance Black Queen $ Position 2 1
          moves = legalMoves pi board
      in(moves `shouldNotContain` [kingPos])
    it "should not be able to jump over an enemyOccupied field." $
      let enemyOccPos = Position 4 6
          board = [PieceInstance White Rook enemyOccPos]
          pi = PieceInstance Black King $ Position 4 5
          moves = legalMoves pi board
      in ((length moves `shouldBe` 8) >> ((map return moves) `shouldNotContain` [positionPlusY enemyOccPos 1]))
    it "should not have it's range extend beyond a selfoccupied field." $
      let enemyOccPos = Position 4 6
          board = [PieceInstance Black Rook enemyOccPos]
          pi = PieceInstance Black King $ Position 4 5
          moves = legalMoves pi board
      in ((length moves `shouldBe` 7) >> (moves `shouldNotContain` [enemyOccPos]))


  describe "Chess.legalMoves Knight" $ do
    it "returns a list of 8 positions in the middle of an empty board." $
      let board = []
          pi = PieceInstance Black Knight $ Position 4 4
          moves = legalMoves pi board
      in (length moves `shouldBe` 8)
    it "returns a list of 7 positions when one field is selfOccupied." $
      let occPos = Position 3 2
          board = [PieceInstance Black Queen occPos]
          pi = PieceInstance Black Knight $ Position 4 4
          moves = legalMoves pi board
      in ((length moves `shouldBe` 7) >> (moves `shouldNotContain` [occPos]))
    it "can never occupy any field that is occupied by a king." $
      let kingPos = Position 3 2
          board = [PieceInstance White King kingPos]
          pi = PieceInstance Black Knight $ Position 4 4
          moves = legalMoves pi board
      in (moves `shouldNotContain` [kingPos])
    it "can take an enemy piece." $
      let enemyOccPos = Position 4 4
          board = [PieceInstance White Queen enemyOccPos]
          pi = PieceInstance Black Knight $ Position 3 2
          moves = legalMoves pi board
      in (moves `shouldContain` [enemyOccPos])


  describe "Chess.legalMoves Rook" $ do
    it "returns a list of 14 positions on an empty board." $
      let board = []
          pi = PieceInstance Black Rook $ Position 1 1
          moves = legalMoves pi board
      in (length moves `shouldBe` 14)
    it "returns a list of 7 positions when one field is selfOccupied, blocking an entire line." $
      let occPos = Position 1 2
          board = [PieceInstance Black Queen occPos]
          pi = PieceInstance Black Rook $ Position 1 1
          moves = legalMoves pi board
      --Available moves should be 7, because the entire vertical line is blocked.
      in ((length moves `shouldBe` 7) >> (moves `shouldNotContain` [occPos]))
    it "can not occupy any field that is occupied by a king." $
      let kingPos = Position 1 2
          board = [PieceInstance White King kingPos]
          pi = PieceInstance Black Rook $ Position 1 1
          moves = legalMoves pi board
      in (moves `shouldNotContain` [kingPos])
    it "returns a list of 14 positions for any empty board start position." $
      let board = []
          pinstances = map (PieceInstance Black Rook) allPositions
          moves :: [(PieceInstance, Int)]
          moves = tuplizeMap (length . legalMovesF board) pinstances
          assertions = map(\(p,l) -> (assertEqual ("For " ++ (show p)) 14) l) moves
      in (sequence_(assertions)) -- use sequence_ to discard all values that the actions result in
    it "should not have it's range extend beyond a selfOccupied field." $
      let selfOccPos = Position 2 2
          board = [PieceInstance Black Pawn selfOccPos]
          pinstance = PieceInstance Black Rook $ Position 2 8
          moves = legalMoves pinstance board
      in (length moves `shouldBe` 12)
    it "should not be able to jump over an enemyOccupied field." $
      let enemyOccPos = Position 2 2
          board = [PieceInstance White Pawn enemyOccPos]
          pinstance = PieceInstance Black Rook $ Position 2 1
          moves = legalMoves pinstance board
      in (
          (length moves `shouldBe` 8) >>
          (moves `shouldContain` [enemyOccPos]) >>
          (moves `shouldNotContain` [(Position 2 3)])) --TODO this is to magic nummery, make nice adders and subtr.


  describe "Chess.legalMoves Bisshop"  $ do
    it "should return at least 7 positions but not more than 14 \nfor any position on an empty board." $
      let board = []
          pis = map (PieceInstance Black Bisshop) allPositions
          moves = tuplizeMap (\p -> legalMovesF board p) pis
          assertion = \x -> length x >= 7 && length x <= 14
          assertions = map (\(pi :: PieceInstance, mov :: [Position]) ->
                              assertBool ("For " ++ (show pi) ++ "we expected the number of moves to be >= 7, was " ++ (show $ length mov))
                              (assertion mov)) moves
      in (sequence_(assertions))
    it "can not occupy a field that is occupied by a king." $
      let kingPos = Position 2 2
          board = [PieceInstance White King kingPos]
          pi = PieceInstance Black Bisshop $ Position 4 4
          moves = legalMoves pi board
      in (moves `shouldNotContain` [kingPos])
    it "should not be able to jump ove an enemyOccupied field." $
      let enemyOccPos = Position 3 3
          board = [PieceInstance White Knight enemyOccPos]
          pi = PieceInstance Black Bisshop $ Position 2 2
          moves = legalMoves pi board
      in((moves `shouldNotContain` [Position 5 5]) >> (length moves `shouldBe` 4))
    it "should not have it's range extended beyond a selfOccupied field." $
      let selfOccPos = Position 3 3
          board = [PieceInstance Black Pawn selfOccPos]
          pi = PieceInstance Black Bisshop $ Position 2 2
          moves = legalMoves pi board
      in ((moves `shouldNotContain` [Position 5 5]) >> (length moves `shouldBe` 3))


  describe "Chess.legalMoves Knight" $ do
    it "should return 8 positions in the middle of an empty board." $
      let board = []
          pi = PieceInstance Black Knight $ Position 4 4
          moves = legalMoves pi board
      in (length moves `shouldBe` 8)
    it "returns a list of 7 positions when one field is selfOccupied." $
      let selfPos = Position 3 2
          board = [PieceInstance Black Pawn selfPos]
          pi = PieceInstance Black Knight $ Position 4 4
          moves = legalMoves pi board
      in ((length moves `shouldBe` 7) >> (moves `shouldNotContain` [selfPos]))
    it "can never occupy a filed that is occupied by a king" $
      let kingPos = Position 3 2
          board = [PieceInstance White King kingPos]
          pi = PieceInstance Black Knight $ Position 4 4
          moves = legalMoves pi board
      in (moves `shouldNotContain` [kingPos])


  describe "Chess.legalMoves Pawn" $ do
    it "should move in one forward direction when White, \non an empty board, have 6 possible consecutive moves." $
      let board = []
          pi = PieceInstance White Pawn
          positions = map pi $ catMaybes $ map (consPosM 1) [2,3,4,5,6,7]
          moves = tuplizeMap (legalMovesF board) positions
          expectedPos = (\pi -> flip positionPlusY 1 $ positionExtractor pi)
          assertions = map (\(pi, mov) -> assertBool
                               ("For " ++ show pi ++ "an expected position was " ++ (show $ expectedPos pi) ++ " got " ++ (show $ legalMoves pi board)) $
                                isJust $ (expectedPos pi >>= (\s -> find (\q -> q == s) mov))) moves
      in(sequence_(assertions) >> (length assertions `shouldBe` 6))
    it "should move in one forward direction when Black, \non an empty board, have 6 possible consecutive moves." $
      let board = []
          pi = PieceInstance Black Pawn
          positions = map pi $ catMaybes $ map (consPosM 1) [7,6,5,4,3,2]
          moves = tuplizeMap (legalMovesF board) positions
          expectedPos = (\pi -> flip positionPlusY (-1) $ positionExtractor pi)
          assertions = map (\(pi, mov) -> assertBool
                               ("For " ++ show pi ++ "an expected position was " ++ (show $ expectedPos pi) ++ " got " ++ (show $ legalMoves pi board)) $
                                isJust $ (expectedPos pi >>= (\s -> find (\q -> q == s) mov))) moves
      in(sequence_(assertions) >> (length assertions `shouldBe` 6))
    it "should be able to hit enemy pieces left and right when White." $
      let board = [PieceInstance Black Knight $ Position 3 6,
                   PieceInstance Black Bisshop $ Position 1 6]
          pi = PieceInstance White Pawn $ Position 2 5
          moves = legalMoves pi board
      in ((length moves `shouldBe` 3) >>
          (moves `shouldContain` (map positionExtractor board))
         )
    it "should be able to hit enemy pieces left and right when Black." $
      let board = [PieceInstance White Knight $ Position 3 2,
                   PieceInstance White Bisshop $ Position 1 2]
          pi = PieceInstance Black Pawn $ Position 2 3
          moves = legalMoves pi board
      in ((length moves `shouldBe` 3) >>
          (moves `shouldContain` (map positionExtractor board)))
    it "can not occupy a field that is occupied by a king \n(in forward and taking diagonal directions)." $
     let board = [PieceInstance Black King $ Position 4 4,
                  PieceInstance White King $ Position 6 4]
         piB = PieceInstance Black Pawn $ Position 5 5
         piW = PieceInstance White Pawn $ Position 5 3
         movesB = legalMoves piB board
         movesW = legalMoves piW board
     in ((assertEqual ("For black there should only be 1 reachable position" ++ show movesB) (length movesB) 1) >>
         (assertEqual "For black the only option should be to move forward"
          (return $ head movesB) (flip positionPlusY (-1) $ positionExtractor piB )) >>
         (assertEqual "For white there should only be 1 reachable position" (length movesW) 1) >>
         (assertEqual "For white the only option should be to move forward"
          (return $ head movesW) (flip positionPlusY 1 $ positionExtractor piW))
        )


  describe "Chess.legalMoves Queen" $ do
    it "should give at least 14 positions and max 27 for any position on an empty field." $
      let board = []
          pis = map (PieceInstance Black Queen) allPositions
          moves :: [(PieceInstance, [Position])] = tuplizeMap (legalMovesF board) pis
          assertion = \x -> length x >= 14 && length x <= 27
          assertions = map (\(pi, mov) -> assertBool
                             ("For " ++ (show pi) ++ " expected movecount to be >= 14 <=27 was " ++ (show $ length mov))
                             (assertion mov)) moves
      in (sequence_(assertions))
    it "can not occupy a field that is occupied by a king." $
      let kingPos = Position 2 2
          board = [PieceInstance White King kingPos]
          pi = PieceInstance Black Queen $ Position 2 1
          moves = legalMoves pi board
      in(moves `shouldNotContain` [kingPos])
    it "should not be able to jump over an enemyOccupied field" $
      let enemyOccPos = Position 4 6
          board = [PieceInstance White Rook enemyOccPos]
          pi = PieceInstance Black Queen $ Position 4 1
          moves = legalMoves pi board
      in ((length moves `shouldBe` 19) >> ((map return moves) `shouldNotContain` [positionPlusY enemyOccPos 1]))
    it "should not have it's range extend beyond a selfoccupied field" $
      let enemyOccPos = Position 4 6
          board = [PieceInstance Black Rook enemyOccPos]
          pi = PieceInstance Black Queen $ Position 4 1
          moves = legalMoves pi board
      in ((length moves `shouldBe` 18) >> (moves `shouldNotContain` [enemyOccPos]))




main :: IO()
main = hspec spec
