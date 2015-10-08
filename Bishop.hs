module Bishop (validPath,noObstacle) where

convert (Just x) = show x 
convert Nothing = " "


validPath x1 y1 x2 y2 b = do
			if abs(x1 - x2) == abs(y1 - y2) 
				then noObstacle x1 y1 x2 y2 b
				else False	


noObstacle x1 y1 x2 y2 b = do
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
