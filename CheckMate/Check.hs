module CheckMate.Check (searchKing,checkKing) where

import Data.List


convert (Just x) = show x
convert Nothing = " "


--Piece{color=White,player=King}= "\9812"
--Piece{color=White,player=Queen} = "\9813"
--Piece{color=White,player=Bishop} = "\9815"
--Piece{color=White,player=Rook} = "\9814"
--Piece{color=White,player=Knight} = "\9816"
--Piece{color=White,player=Pawn} = "\9817"
--Piece{color=Black,player=King} = "\9818"
--Piece{color=Black,player=Queen} = "\9819"
--Piece{color=Black,player=Bishop} = "\9821"
--Piece{color=Black,player=Rook} = "\9820"
--Piece{color=Black,player=Knight} = "\9822"
--Piece{color=Black,player=Pawn} = "\9823"

pawnAttack b king_pos piece = do
	let x = fst king_pos
	let y = snd king_pos

	if piece == "\9823"
		then do
			if x > 0
				then do
					if y > 0 && y < 7
						then do
							(convert ((b!!(x-1))!!(y-1))) == piece || (convert ((b!!(x-1))!!(y+1))) == piece
						else do
							if y == 0
								then do
									(convert ((b!!(x-1))!!(y+1))) == piece
								else do
									(convert ((b!!(x-1))!!(y-1))) == piece
				else do False

		else do
			if x < 7
				then do
					if y > 0 && y < 7
						then do
							(convert ((b!!(x+1))!!(y-1))) == piece || (convert ((b!!(x+1))!!(y+1))) == piece
						else do
							if y == 0
								then do
									(convert ((b!!(x+1))!!(y+1))) == piece
								else do
									(convert ((b!!(x+1))!!(y-1))) == piece
				else do False


rookAttack b king_pos piece = do
	let x = fst king_pos
	let y = snd king_pos

	let check1 x' | x' >= 0 && (convert ((b!!x')!!y)) == piece = True
				  | x' >= 0 && (convert ((b!!x')!!y)) == " " = check1 (x' - 1)
				  | otherwise 				= False
	let c1 = check1 (x - 1)

	let check2 x' | x' <= 7 && (convert ((b!!x')!!y)) == piece = True
				  | x' <= 7 && (convert ((b!!x')!!y)) == " " = check2 (x' + 1)
				  | otherwise 				= False
	let c2 = check2 (x + 1)

	let check3 y' | y' >= 0 && (convert ((b!!x)!!y')) == piece = True
				  | y' >= 0 && (convert ((b!!x)!!y')) == " " = check3 (y' - 1)
				  | otherwise 				= False
	let c3 = check3 (y - 1)

	let check4 y' | y' <= 7 && (convert ((b!!x)!!y')) == piece = True
				  | y' <= 7 && (convert ((b!!x)!!y')) == " " = check4 (y' + 1)
				  | otherwise 				= False
	let c4 = check4 (y + 1)

	(c1 || c2 || c3 || c4)

bishopAttack b king_pos piece = do
	let x = fst king_pos
	let y = snd king_pos

	let check1 x' y' | x' >= 0 && y' >= 0 && (convert ((b!!x')!!y')) == piece = True
				  	 | x' >= 0 && y' >= 0 && (convert ((b!!x')!!y')) == " " = check1 (x' - 1) (y' - 1)
				  	 | otherwise 				= False
	let c1 = check1 (x - 1) (y - 1)

	let check2 x' y' | x' <= 7 && y' >= 0 && (convert ((b!!x')!!y')) == piece = True
				  	 | x' <= 7 && y' >= 0 && (convert ((b!!x')!!y')) == " " = check2 (x' + 1) (y' - 1)
				  	 | otherwise 				= False
	let c2 = check2 (x + 1) (y - 1)

	let check3 x' y' | x' >= 0 && y' <= 7 && (convert ((b!!x')!!y')) == piece = True
				  	 | x' >= 0 && y' <= 7 && (convert ((b!!x')!!y')) == " " = check3 (x' - 1) (y' + 1)
				  	 | otherwise 				= False
	let c3 = check3 (x - 1) (y + 1)

	let check4 x' y' | x' <= 7 && y' <= 7 && (convert ((b!!x')!!y')) == piece = True
				  	 | x' <= 7 && y' <= 7 && (convert ((b!!x')!!y')) == " " = check4 (x' + 1) (y' + 1)
				  	 | otherwise 				= False
	let c4 = check4 (x + 1) (y + 1)

	(c1 || c2 || c3 || c4)


queenAttack b king_pos piece = do
	let c1 = rookAttack b king_pos piece
	let c2 = bishopAttack b king_pos piece
	(c1 || c2)


knightAttack b king_pos piece = do
	let x = fst king_pos
	let y = snd king_pos

	let c1 = if (x-2) >= 0
				then do 
					if y > 0 && y < 7
						then do 
							(convert ((b!!(x-2))!!(y-1))) == piece || (convert ((b!!(x-2))!!(y+1))) == piece
						else do
							if y == 0
								then do 
									(convert ((b!!(x-2))!!(y+1))) == piece
								else do
									(convert ((b!!(x-2))!!(y-1))) == piece
				else do False

	let c2 = if (x+2) <= 7
				then do 
					if y > 0 && y < 7
						then do 
							(convert ((b!!(x+2))!!(y-1))) == piece || (convert ((b!!(x+2))!!(y+1))) == piece
						else do
							if y == 0
								then do 
									(convert ((b!!(x+2))!!(y+1))) == piece
								else do
									(convert ((b!!(x+2))!!(y-1))) == piece
				else do False
	
	let c3 = if (y-2) >= 0
				then do 
					if x > 0 && x < 7
						then do 
							(convert ((b!!(x-1))!!(y-2))) == piece || (convert ((b!!(x+1))!!(y-2))) == piece
						else do
							if x == 0
								then do 
									(convert ((b!!(x+1))!!(y-2))) == piece
								else do
									(convert ((b!!(x-1))!!(y-2))) == piece
				else do False

	let c4 = if (y+2) <= 7
				then do 
					if x > 0 && x < 7
						then do 
							(convert ((b!!(x-1))!!(y+2))) == piece && (convert ((b!!(x+1))!!(y+2))) == piece
						else do
							if x == 0
								then do 
									(convert ((b!!(x+1))!!(y+2))) == piece
								else do
									(convert ((b!!(x-1))!!(y+2))) == piece
				else do False

	(c1 || c2 || c3 || c4)



kingAttack b king_pos piece = do
	let x = fst king_pos
	let y = snd king_pos

	let c1 = x > 0 && (convert ((b!!(x-1))!!y)) == piece
	let c2 = x < 7 && (convert ((b!!(x+1))!!y)) == piece
	let c3 = y > 0 && (convert ((b!!x)!!(y-1))) == piece
	let c4 = y < 7 && (convert ((b!!x)!!(y+1))) == piece

	let c5 = x > 0 && y > 0 && (convert ((b!!(x-1))!!(y-1))) == piece
	let c6 = x > 0 && y < 7 && (convert ((b!!(x-1))!!(y+1))) == piece
	let c7 = x < 7 && y > 0 && (convert ((b!!(x+1))!!(y-1))) == piece
	let c8 = x < 7 && y < 7 && (convert ((b!!(x+1))!!(y+1))) == piece

	(c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8)


searchKing b king_color = do
	let b' = (map.map) convert b
	-- king_color determines the color of pawn : 1 if White, Black if -1
	if king_color == 1
		then do
			let king_pos i | "\9812" `elem` (b'!!i) = do
												let x = (\(Just x)->x) $ elemIndex "\9812" (b'!!i)
												(i,x)
					  | otherwise   		   = do king_pos (i+1)
			king_pos 0

		else do
			let king_pos i | "\9818" `elem` (b'!!i) = do
												let x = (\(Just x)->x) $ elemIndex "\9818" (b'!!i)
												(i,x)
					  | otherwise   		  = do king_pos (i+1)
			king_pos 0


checkKing b king_color = do
	let king_pos = searchKing b king_color

	-- king_color determines the color of pawn : 1 if White, Black if -1
	let c1 = pawnAttack b king_pos (if king_color == 1 then "\9823" else "\9817")
	let c2 = rookAttack b king_pos (if king_color == 1 then "\9820" else "\9814")
	let c3 = bishopAttack b king_pos (if king_color == 1 then "\9821" else "\9815")
	let c4 = queenAttack b king_pos (if king_color == 1 then "\9819" else "\9813")
	let c5 = knightAttack b king_pos (if king_color == 1 then "\9822" else "\9816")
	let c6 = kingAttack b king_pos (if king_color == 1 then "\9818" else "\9812")

	(c1 || c2 || c3 || c4 || c5 || c6)
