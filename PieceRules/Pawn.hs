module PieceRules.Pawn (validPath) where

convert (Just x) = show x
convert Nothing = " "


validPath x1 y1 x2 y2 b colmarker = do
	-- colmarker determines the color of pawn : 1 if White, Black if -1
			if colmarker == -1
				then do
					if ((x2 - x1) == 1 && y1 == y2) || ((x2 - x1) == 2 && y1 == y2) || ((x2 - x1) == 1 && abs(y1 - y2) == 1)
						then do
							if (x2 - x1) == 2 && y1 == y2
								then firstMove_B x1 y1 x2 y2 b
								else correctMove x1 y1 x2 y2 b
						else do
							False
				else do
					if ((x1 - x2) == 1 && y1 == y2) || ((x1 - x2) == 2 && y1 == y2) || ((x1 - x2) == 1 && abs(y1 - y2) == 1)
						then do
							if (x1 - x2) == 2 && y1 == y2
								then firstMove_W x1 y1 x2 y2 b
								else correctMove x1 y1 x2 y2 b
						else do
							False

firstMove_B x1 y1 x2 y2 b = if x1 == 1 then True else False
firstMove_W x1 y1 x2 y2 b = if x1 == 6 then True else False

correctMove x1 y1 x2 y2 b = do
			if y1 == y2
				then do
					if (convert ((b!!x2)!!y2)) == " " then True else False
				else do
					if (convert ((b!!x2)!!y2)) /= " " then True else False
