module Apoc where

type Board = [[Square]]
initialBoardStr :: String
initialBoardStr = unlines ["-----------"
						  ,"|X|/|/|/|X|"
						  ,"|/|_|_|_|/|"
						  ,"|_|_|_|_|_|"
						  ,"|+|_|_|_|+|"
						  ,"|#|+|+|+|#|"
						  ,"-----------"
						  ]
type Square = Maybe Piece


showSquare :: Square -> Char
showSquare = maybe '_' showPiece

readSquare :: Char -> Square
readSquare c = readPiece c

readBoard :: String -> Board
readBoard = map readRow . lines
	where readRow = map readSquare

showBoard :: Board -> String
showBoard = unlines . map showRow
	where showRow = map showSquare

data Piece = Piece PColor PType
data PColor = Black | White
data PType = Pawn | Knight

showPiece :: Piece -> Char
showPiece (Piece White Pawn) = '+'
showPiece (Piece White Knight) = '#'
showPiece (Piece Black Pawn) = '/'
showPiece (Piece Black Knight) = 'X'


typeList :: [(Char, PType)]
typeList = [('/', Pawn)
		   ,('+', Pawn)
		   ,('X', Knight)
		   ,('#', Knight)
		   ]

readPiece :: Char -> Maybe Piece
readPiece '+' = Just (Piece White Pawn)
readPiece '#' = Just (Piece White Knight)
readPiece '/' = Just (Piece Black Pawn)
readPiece 'X' = Just (Piece Black Knight)
readPiece _ = Nothing
