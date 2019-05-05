{-# LANGUAGE ScopedTypeVariables #-}
module Server
  (
  ) where

import Data.Maybe
import Chess

data NewGame

data Move

data GetMoves

data Error = Error String deriving (Show, Eq, Ord)

epos2fepos :: Position -> String
epos2fepos p =
  let (a :: Char, i :: Int) = Chess.humanReadablePosition(p)
      in ("" ++ [a] ++ show i)

feps2epos :: String -> Maybe Position
feps2epos s = pure $ Position 1 1

board2fen :: Board -> String
board2fen b = ""

-- API
getPossibleMoves :: String -> [String]
getPossibleMoves s = undefined

move :: String -> Either Board Error
move s = undefined

start :: NewGame -> String
start n = undefined


