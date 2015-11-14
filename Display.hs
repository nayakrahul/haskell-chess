module Display (prints) where
import qualified System.Console.ANSI as S

prints::[String] -> Bool -> Int -> IO()
prints a t r = do
	if t == True
		then do
			putStr ((show r)++" ")
			S.setSGR [S.SetColor S.Foreground S.Dull S.Black]
			S.setSGR [S.SetColor S.Background S.Vivid S.White]
			putStr (" "++(a!!0)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.Black]
			putStr (" "++(a!!1)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.White]
			putStr (" "++(a!!2)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.Black]
			putStr (" "++(a!!3)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.White]
			putStr (" "++(a!!4)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.Black]
			putStr (" "++(a!!5)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.White]
			putStr (" "++(a!!6)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.Black]
			putStr (" "++(a!!7)++" ")
			S.setSGR [S.Reset]
			putStr (" "++(show r))
			putStr "\n"
		else do
			putStr ((show r)++" ")
			S.setSGR [S.SetColor S.Foreground S.Dull S.Black]
			S.setSGR [S.SetColor S.Background S.Vivid S.Black]
			putStr (" "++(a!!0)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.White]
			putStr (" "++(a!!1)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.Black]
			putStr (" "++(a!!2)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.White]
			putStr (" "++(a!!3)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.Black]
			putStr (" "++(a!!4)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.White]
			putStr (" "++(a!!5)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.Black]
			putStr (" "++(a!!6)++" ")
			S.setSGR [S.SetColor S.Background S.Vivid S.White]
			putStr (" "++(a!!7)++" ")
			S.setSGR [S.Reset]
			putStr (" "++(show r))
			putStr "\n"