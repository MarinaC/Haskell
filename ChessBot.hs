{-# LANGUAGE OverloadedStrings #-}
module ChessBot where

import Data.Char
import Data.List.Split


-------------------------------------------
--    Example Boards and Board validation
-------------------------------------------

-- this is the start board layout
startFEN :: String
startFEN = "rnbkqbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBKQBNR"

-- an example of a board where one player is checkmate
checkMateFEN :: String
checkMateFEN = "3K3q/7r/8/8/8/8/7k/8"

-- checks if a FEN string really contains a valid chess board (8x8 fields)
validateFENString :: String -> Bool
validateFENString s = let rows = splitFENString s in
                    length rows == 8 && -- need 8 rows
                        foldr (((&&) . (== 8)) . countFigures) True rows -- 8 columns per row
                        
-- counts the fields of a string (a row)
countFigures :: String -> Int
countFigures [] = 0
countFigures s = let c = head s in  
                 if isDigit c then digitToInt c + countFigures (tail s)
                 else 
                    if c=='r' || c=='R' || c=='b' || c=='B' || c=='n' || c=='N' || c=='q' || c=='Q' || c=='k' || c=='K' || c=='p' || c=='P' then 
                        1 + countFigures (tail s)
                    else 
                        9 --more than eight means bad input

-- splits a string at the "/" -> converts a FEN string field to a list of FEN string rows
splitFENString :: String -> [String]
splitFENString = splitOn "/" 

-----------------------------------------
--    Bot logic
-----------------------------------------

<<<<<<< HEAD


--returns list with name/number of the field occupied with a piece (Jori)
-- 
--Param1: FENString
--Param2: Piece Char 
searchPiece :: String -> char -> [Int]
searchPiece fen x = if elemIndexL x (splitFENString fen) == elemIndexR x (splitFENString fen) then elemIndexR x (splitFENString fen)
							else if elemIndexL x (splitFENString fen) == Nothing then []
								else elemIndexL x (splitFENString fen) ++ elemIndexR x (splitFENString fen) 


								
								
-- returns list of all possible rook moves
--Param1: FENString
--Param2: Position
knightMoves :: String -> Int -> [Int]
knightMoves fen pos = [square|steps = [7,-7,17,-17,15,-15,10,-10] let square = pos + steps, colorTest fen pos square]


--returns if target square has different or no color
--Param1:FENString
--Param2: Position1
--Param2: Position2
difColorTest :: String -> Int -> Int -> Bool
difColorTest fen pos1 pos2 = let isUpper (getPiece fen pos1) = isWhite in if isWhite == isUpper(getPiece fen pos2) then true else false

-- returns Char on the Int position
--Param1: FENString
--Param2: Position
getPiece :: String -> Int -> Char
getPiece fen pos =  fen !! pos


=======
--number to list of 1
unfoldNumber :: Int -> String
unfoldNumber 0 = []
unfoldNumber x = '1' : (unfoldNumber (x-1))

-- unfold fen string
unfoldFen :: String -> String
unfoldFen [] = []
unfoldFen (x:xs)= if isDigit x then unfoldNumber (digitToInt x) ++ ( unfoldFen xs) else unfoldFen xs

--TODO unfolded fen to fen
--foldFen :: String -> String
--foldFen [] = []
--foldFen (x:xs) = 

--get position of figure
getSource :: Char -> String -> Int -> Int
getSource _ [] 65 = 0
getSource _ [] y = y
-- position: 1-64
getSource piece (x:xs) y = if x == piece then y else getSource piece xs y+1

--int to char: a b c d e f g h
intToChar :: Int -> Char
intToChar x = if (elem x [1,9..57]) then 'a' else 
 if (elem x [2,10..58]) then 'b' else 
 if (elem x [3,11..59]) then 'c' else
 if (elem x [4,12..60]) then 'd' else
 if (elem x [5,13..61]) then 'e' else
 if (elem x [6,14..62]) then 'f' else
 if (elem x [7,15..63]) then 'g' else
 if (elem x [8,16..64]) then 'h' else 'x'


--int position to string position
getPosition :: Int -> String
getPosition x = if (elem x [1..8]) then (intToChar x) : "8" else
 if (elem x [9..16]) then (intToChar x) : "7" else
 if (elem x [17..24]) then (intToChar x) : "6" else
 if (elem x [25..32]) then (intToChar x) : "5" else
 if (elem x [33..40]) then (intToChar x) : "4" else
 if (elem x [41..48]) then (intToChar x) : "3" else
 if (elem x [49..56]) then (intToChar x) : "2" else
 if (elem x [57..64]) then (intToChar x) : "1" else "error"

>>>>>>> origin/master
-- returns a list of all valid moves for the given board and color
-- parameter 1: first part of FEN String
-- parameter 2: "w"/"b" 
botListAllMoves :: String -> String -> [String]
botListAllMoves board color = "a7 b6":"a7 c5":[] -- TODO: replace this with the real implementation

-- returns one valid move in the format [source target]
-- parameter 1: first part of FEN String
-- parameter 2: "w"/"b" 
botFindMove :: String -> String -> String
botFindMove board color = "a7 b6" -- TODO: replace this with your program     
              


