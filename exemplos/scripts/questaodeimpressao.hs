import System.IO.Unsafe

gere_string [] = ""
gere_string x = let (a,b,c,d) = head x in 
			    (show a) ++ gere_string (tail x)
			
comportamento1 l = do 
					print (gere_string l);
					return 10
				  
comportamento2 l = unsafePerformIO (comportamento1 l)

{- comportamento3 funciona como vari�vel, n�o como fun��o -}
comportamento3 = comportamento2 l

l = [("a1", "b1", "c1", "d1"), ("a2", "b2", "c2", "d2")]