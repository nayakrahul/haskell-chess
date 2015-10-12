module Display (prints) where
import qualified System.Console.ANSI as S

prints::[String] -> Bool -> IO()
prints a t = do
	if t == True
		then do
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
			putStr "\n"
		else do
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
			putStr "\n"