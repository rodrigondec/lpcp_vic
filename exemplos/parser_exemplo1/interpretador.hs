import System.IO
import System.IO.Unsafe

program :: [String] -> (Bool, [String])
program (c0:t0) = if c0 /= "program" then (False, t0)
	else
		let (s1, c1:t1) = idd t0 in
			if s1 == False then (False, c1:t1)
			else
				let (s2, c2:t2) = var_section (c1:t1) in
					if s2 == False then (False, c2:t2)
					else 
						if c2 /= "begin" then (False, t2) 
						else
							let (s3, c3:t3) = stmts t2 in
								if s3 == False then (False, c3:t3)
								else
								   if c3 /= "end" then (False, t3) 
						   		   else (True,[])

idd :: [String] -> (Bool, [String])
idd (c0:t0) = (True, t0)

var_section :: [String] -> (Bool, [String])
var_section (c0:t0) = if c0 == "begin" then (True, c0:t0)
					  else var_section t0

stmts :: [String] -> (Bool, [String])
stmts (c0:t0) = if c0 == "end" then (True, c0:t0)
 				else stmts t0
 				
main :: String -> (Bool, [String])
main fn = program (unsafePerformIO (leia fn))

leia fn = do x <- openFile fn ReadMode
             y <- hGetContents x
             return (words y)