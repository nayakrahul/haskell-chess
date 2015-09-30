import Data.Foldable 
import Data.Sequence

type Board = [[Maybe Piece]]

data Piece = Piece{color::Color,player::Player} 
data Color = White | Black 
data Player = King | Queen | Rook | Knight | Bishop | Pawn 

instance Show Color where
	show White = "White"
	show Black = "Black" 

instance Eq Color where
	White == White = True
	Black == Black = True
	_ == _ = False

instance Show Player where
	show King = "King"
	show Queen = "Queen"
	show Rook = "Rook"
	show Knight = "Knight"
	show Bishop = "Bishop"
	show Pawn = "Pawn" 

instance Show Piece where
	show Piece{color=White,player=King}= "WK"
	show Piece{color=White,player=Queen} = "WQ"
	show Piece{color=White,player=Bishop} = "WB"
	show Piece{color=White,player=Rook} = "WR"
	show Piece{color=White,player=Knight} = "WN"
	show Piece{color=White,player=Pawn} = "WP"
	show Piece{color=Black,player=King} = "Bk"
	show Piece{color=Black,player=Queen} = "BQ"
	show Piece{color=Black,player=Bishop} = "BB"
	show Piece{color=Black,player=Rook} = "BR"
	show Piece{color=Black,player=Knight} = "BN"
	show Piece{color=Black,player=Pawn} = "BP"
	show _ = ""

wk = Piece{color=White,player=King}
wq = Piece{color=White,player=Queen}
wb = Piece{color=White,player=Bishop}
wr = Piece{color=White,player=Rook}
wn = Piece{color=White,player=Knight}
wp = Piece{color=White,player=Pawn}
bk = Piece{color=Black,player=King}
bq = Piece{color=Black,player=Queen}
bb = Piece{color=Black,player=Bishop}
br = Piece{color=Black,player=Rook}
bn = Piece{color=Black,player=Knight}
bp = Piece{color=Black,player=Pawn}

initialBoard :: Board
initialBoard = [[Just wr,Just wn,Just wb,Just wq,Just wk,Just wb,Just wn, Just wr],
				[Just wp,Just wp,Just wp,Just wp,Just wp,Just wp,Just wp, Just wp],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
				[Just bp,Just bp,Just bp,Just bp,Just bp,Just bp,Just bp, Just bp],
				[Just br,Just bn,Just bb,Just bq,Just bk,Just bb,Just bn, Just br]]


change::Int -> Int -> Int -> Int -> Board -> Board
change x1 y1 x2 y2 a = do 
		let p = (a!!x1)!!y1
		let b2 | x1 == x2  = toList $ update y1 Nothing $ fromList (toList $ update y2 p $ fromList (a!!x2))
			   | otherwise = toList $ update y2 p $ fromList (a!!x2)
		let b1 | x1 ==x2   = toList $ update y1 Nothing $ fromList (toList $ update y2 p $ fromList (a!!x2))
			   | otherwise = toList $ update y1 Nothing $ fromList (a!!x1)
		let f = \x -> case () of () | x == x1 -> b1 | x == x2 -> b2 |otherwise -> a!!x
		let b = ([f 0] ++ [f 1] ++ [f 2] ++ [f 3] ++ [f 4] ++ [f 5] ++ [f 6] ++ [f 7])
		b

move::[String] -> Board -> Bool -> IO()
move x b chance = do
		let a1 = (read (x!!0)::Int)
	 	let a2 = (read (x!!1)::Int)
	 	let a3 = (read (x!!2)::Int)
	 	let a4 = (read (x!!3)::Int)
	 	let a5 = a1 >= 0 && a1 <= 7 && a2 >= 0 && a2 <= 7 && a3 >= 0 && a3 <= 7 && a4 >= 0 && a4 <= 7
	 	if a5
	 		then do
			 	let col = color $ (\(Just x) -> x) ((b!!a1)!!a2)
			 	if( ((chance == True && col == White) || (chance == False && col == Black)) && a5 )
			 		then do
			 			let board = change a1 a2 a3 a4 b
			 			let board' = (map.map) convert board
			 			let print' n | n == 7    = print ((board'!!n))
			 					 	 | otherwise = do 
			 			  						print ((board'!!n))
			 			  						print' (n+1)
			 			print' 0
			 			c <- getLine
			 			let x = (words $ c)
			 			move x board (not chance)
			 		else do 
			 			putStrLn "Invalid Move - Play again"
			 			c <- getLine
			 			let x = (words $ c)
			 			move x b (chance)
	 	else do
	 		putStrLn "Invalid Input - Play again"
			c <- getLine
			let x = (words $ c)
			move x b (chance)

convert::Maybe Piece -> String 
convert (Just x) = show x 
convert Nothing = "  "

main = do
	 let board' = (map.map) convert initialBoard
	 let print' n | n == 7    = print ((board'!!n))
	 					 | otherwise = do 
	 			  						print ((board'!!n))
	 			  						print' (n+1)
	 print' 0
	 c <- getLine
	 let x = (words $ c)
	 -- True -> White to play, False -> Black to play
	 let chance = True
	 let a1 = (read (x!!0)::Int)
	 let a2 = (read (x!!1)::Int)
	 let a3 = (read (x!!2)::Int)
	 let a4 = (read (x!!3)::Int)
	 let a5 = a1 >= 0 && a1 <= 7 && a2 >= 0 && a2 <= 7 && a3 >= 0 && a3 <= 7 && a4 >= 0 && a4 <= 7
	 if a5
	 	then do
	 		let col = color $ (\(Just x) -> x) ((initialBoard!!a1)!!a2)
	 
	 		if( chance == True && col == White && a5 )
	 			then do 
	 				let board = change a1 a2 a3 a4 initialBoard
			 		let board' = (map.map) convert board
			 		let print' n | n == 7    = print ((board'!!n))
			 					 | otherwise = do 
			 			  						print ((board'!!n))
			 			  						print' (n+1)
			 		print' 0
			 		c <- getLine
			 		let x = (words $ c)
			 		move x board (not chance)
			 	else do
			 		putStrLn "Invalid Move - White to play - Play again"
			 		c <- getLine
			 		let x = (words $ c)
			 		move x initialBoard (chance)
		else do
			putStrLn "Invalid Input - Play again"
			c <- getLine
			let x = (words $ c)
			move x initialBoard (chance) 
	 
	 --c <- getLine
	 --let x = (words $ c)
	 --move x board 
	 