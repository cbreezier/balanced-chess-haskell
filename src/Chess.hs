module Chess
    ( initBoard
    ) where

import Data.List

data Piece = Pawn
           | Rook
           | Knight
           | Bishop
           | Queen
           | King
           | None
    deriving (Bounded, Enum, Eq, Show)

pieceString :: Piece -> String
pieceString piece
    | piece == Pawn = "P"
    | piece == Rook = "R"
    | piece == Knight = "N"
    | piece == Bishop = "B"
    | piece == Queen = "Q"
    | piece == King = "K"
    | piece == None = " "

data Coord = Coord
    { row :: Int
    , col :: Int
    }
  deriving (Show, Eq)

data Board = Board
    (
        (Piece, Piece, Piece, Piece, Piece, Piece, Piece, Piece),
        (Piece, Piece, Piece, Piece, Piece, Piece, Piece, Piece),
        (Piece, Piece, Piece, Piece, Piece, Piece, Piece, Piece),
        (Piece, Piece, Piece, Piece, Piece, Piece, Piece, Piece),
        (Piece, Piece, Piece, Piece, Piece, Piece, Piece, Piece),
        (Piece, Piece, Piece, Piece, Piece, Piece, Piece, Piece),
        (Piece, Piece, Piece, Piece, Piece, Piece, Piece, Piece),
        (Piece, Piece, Piece, Piece, Piece, Piece, Piece, Piece)
    )
  deriving (Eq)

instance Show Board where
    show (Board (r0, r1, r2, r3, r4, r5, r6, r7)) =
        let rowToString :: (Piece, Piece, Piece, Piece, Piece, Piece, Piece, Piece) -> String
            rowToString (v0, v1, v2, v3, v4, v5, v6, v7) = 
                "|" ++ (pieceString v0) ++
                "|" ++ (pieceString v1) ++
                "|" ++ (pieceString v2) ++
                "|" ++ (pieceString v3) ++
                "|" ++ (pieceString v4) ++
                "|" ++ (pieceString v5) ++
                "|" ++ (pieceString v6) ++
                "|" ++ (pieceString v7) ++
                "|"
            divider :: String
            divider = "+" ++ (intersperse '+' (replicate 8 '-')) ++ "+"
        in
            divider ++ "\n" ++
            (rowToString r0) ++ "\n" ++ divider ++ "\n" ++
            (rowToString r1) ++ "\n" ++ divider ++ "\n" ++
            (rowToString r2) ++ "\n" ++ divider ++ "\n" ++
            (rowToString r3) ++ "\n" ++ divider ++ "\n" ++
            (rowToString r4) ++ "\n" ++ divider ++ "\n" ++
            (rowToString r5) ++ "\n" ++ divider ++ "\n" ++
            (rowToString r6) ++ "\n" ++ divider ++ "\n" ++
            (rowToString r7) ++ "\n" ++ divider ++ "\n"

initBoard :: Board
initBoard = Board
    (
        (Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook),
        (Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn),
        (None, None, None, None, None, None, None, None),
        (None, None, None, None, None, None, None, None),
        (None, None, None, None, None, None, None, None),
        (None, None, None, None, None, None, None, None),
        (Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn),
        (Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook)
    )
