data Simbolo =  Cons (Integer, String, String, String, [String], String)

instance Show Simbolo where
	show (Cons (i,s1,s2,s3,l,s4)) =  "Cons(" ++ show i ++ ", " ++ show s1 ++ ", " ++ 
	                                         show s2 ++ ", " ++ show s3 ++ ", " ++
											 show l ++ ", " ++ show s4 ++ ")"
											 
v1 :: Simbolo
v1 = Cons (1, "S1", "S2", "S3", ["Sub1", "Sub2"], "S4")											 