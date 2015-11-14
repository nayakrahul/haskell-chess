module PieceRules.Queen (validPath) where

convert (Just x) = show x
convert Nothing = " "

validPath x1 y1 x2 y2 b = do
			if abs(x1 - x2) == abs(y1 - y2) || (x1 == x2 && y1 /= y2) || (x1 /= x2 && y1 == y2)
				then noObstacle x1 y1 x2 y2 b
				else False


noObstacle x1 y1 x2 y2 b = do
	if  abs(x1 - x2) == abs(y1 - y2)
		then do
			if x1 < x2
				then do
					if y1 < y2
						then do
							let check x y | x < x2 && y < y2 && (convert ((b!!x)!!y)) /= " " = False
								         | x < x2 && y < y2 && (convert ((b!!x)!!y)) == " " = check (x + 1) (y + 1)
								         | otherwise 					      = True
							check (x1 + 1) (y1 + 1)
						else do
							let check x y | x < x2 && y > y2 && (convert ((b!!x)!!y)) /= " " = False
								         | x < x2 && y > y2 && (convert ((b!!x)!!y)) == " " = check (x + 1) (y - 1)
								         | otherwise 					      = True
							check (x1 + 1) (y1 - 1)
				else do
					if y1 < y2
						then do
							let check x y | x > x2 && y < y2 && (convert ((b!!x)!!y)) /= " " = False
								         | x > x2 && y < y2 && (convert ((b!!x)!!y)) == " " = check (x - 1) (y + 1)
								         | otherwise 					      = True
							check (x1 - 1) (y1 + 1)
						else do
							let check x y | x > x2 && y > y2 && (convert ((b!!x)!!y)) /= " " = False
								         | x > x2 && y > y2 && (convert ((b!!x)!!y)) == " " = check (x - 1) (y - 1)
								         | otherwise 					       = True
							check (x1 - 1) (y1 - 1)
		else do
			if(x1 == x2 && y1 /= y2)
				then do
					if y1 < y2
						then do
							let check y | y < y2 && (convert ((b!!x1)!!y)) /= " "  = False
								      | y < y2 && (convert ((b!!x1)!!y)) == " " = check (y + 1)
								      | otherwise 				= True
							check (y1 + 1)
						else do
							let check y | y > y2 && (convert ((b!!x1)!!y)) /= " " = False
								      | y > y2 && (convert ((b!!x1)!!y)) == " " = check (y - 1)
								      | otherwise 				= True
							check (y1 - 1)
				else do
					if x1 < x2
						then do
							let check x | x < x2 && (convert ((b!!x)!!y1)) /= " " = False
								      | x < x2 && (convert ((b!!x)!!y1)) == " " = check (x + 1)
								      | otherwise 				= True
							check (x1 + 1)
						else do
							let check x | x > x2 && (convert ((b!!x)!!y1)) /= " " = False
								      | x > x2 && (convert ((b!!x)!!y1)) == " " = check (x - 1)
								      | otherwise 				= True
							check (x1 - 1)
