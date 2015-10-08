module Knight (validPath) where

convert (Just x) = show x
convert Nothing = " "


validPath x1 y1 x2 y2 b = do
			if (abs(x1 - x2) == 2 && abs(y1 - y2) == 1) || (abs(x1 - x2) == 1 && abs(y1 - y2) == 2)
				then True
				else False
