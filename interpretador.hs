import System.IO
import System.IO.Unsafe

error_msg :: String -> String
error_msg msg = "Erro: palavra reservada '" ++ msg ++ "' nao encontrada!"



func_main :: [String] -> (Bool, [String])
func_main (c0:t0) = if c0 /= "void" then (False, [error_msg "void"])
					else
						if head t0 /= "main()" then (False, [error_msg "main()"])
						else
							let (s1, c1:t1) = idd t0 in
								if s1 == False then (False, c1:t1)
								else
									let (s2, t2) = func_body (c1:t1) in
									if s2 == False then (False, t2)
									else (True, c0:t0)



func_body :: [String] -> (Bool, [String])
func_body (c:t) = if c /= "{" then (False, t)
				  else linhas t

linhas :: [String] -> (Bool, [String])
linhas (c:t) = if c == "}" then (True, t)
			   else linhas t

--arg_list :: [String] -> (Bool, [String])
-- Ainda falta terminar

idd :: [String] -> (Bool, [String])
idd (c:t) = (True, t)

func_principal :: String -> (Bool, [String])
func_principal arq = func_main (unsafePerformIO (leitor arq))


leitor fn = do x <- openFile fn ReadMode
               y <- hGetContents x
               return (words y)

