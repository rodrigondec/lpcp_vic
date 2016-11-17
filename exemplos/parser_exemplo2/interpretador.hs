import System.IO
import System.IO.Unsafe
import Data.List.Split

type Name = String
data Value = IntV Int | FloatV Float
data Type = Int | Float
type Escope = String
type Symbol = (Name, Value, Type, Escope)
  
instance Show Value where
   show (IntV v) = "IntV" ++ show v
   show (FloatV v) = "FloatV" ++ show v
   
instance Show Type where
   show (Int) = "Int"
   show (Float) = "Float"
   
errorMsg :: String -> String
errorMsg msg = "Error: construct '" ++ msg ++ "' not found."
                         
idd :: [String] -> (Bool, [String])
idd (h:t) = (True, t)

add_symtable :: [Symbol] -> [Name] -> Type -> [Symbol]
add_symtable [] ids Int = [(id, IntV 0, Int, "") | id <- ids]
add_symtable st ids Int = st ++ [(id, IntV 0, Int, "") | id <- ids]
add_symtable [] ids Float = [(id, FloatV 0.0, Float, "") | id <- ids]    
add_symtable st ids Float = st ++ [(id, FloatV 0.0, Float, "") | id <- ids]  

lookup_symtable :: Name -> Int
lookup_symtable _ = 22   

stringToType :: String -> Type
stringToType "int" = Int
stringToType "float" = Float  
stringToType x = error (x ++ " (Type)") 

id_list_aux :: [String] -> [Name] -> ([String],[Name])
id_list_aux (h:t) ids = if h == ":" then (ids, h:t)
                        else id_list_aux t (ids ++ [(filter (/=',') h)])

id_list :: [String] -> (Bool, [String], [Name])
id_list [] = (True, [], [])
id_list (h:t) = let (ids, r) = id_list_aux (h:t) [] in (True, r, ids)
                   
 
var_decl :: [String] -> [Symbol] -> (Bool, [String], [Symbol])
var_decl (h0:t0) st = if h0 == "begin" then (True, h0:t0, st)
                      else 
                       let (s1,r1,l1) = id_list (h0:t0) in 
                         if s1 /= True  then (False, [errorMsg "var_decl"], st)
                         else
                           let (s2:r2) = r1 in
                             if s2 /= ":" then (False, [errorMsg "var_decl"], st)
                             else 
                               let (s3:r3) = r2 in 
                                 var_decl r3 (add_symtable st l1 (stringToType (filter (/=';') s3)))
                     
var_decl_list :: [String] -> [Symbol] -> (Bool, [String], [Symbol])
var_decl_list (h0:t0) st = if h0 == "begin" then (True, h0:t0, st)
                           else 
                             let (s1, r1, st2) = var_decl (h0:t0) st in 
                              if (s1 /=True) then (False, [errorMsg "var_decl"], st2)
                              else var_decl_list r1 st2

var_section :: [String] -> [Symbol] -> (Bool, [String], [Symbol])
var_section (h0:t0) st = if h0 /= "var" then (False, [errorMsg "var"], st)
                         else var_decl_list t0 st

write_stm :: [String] -> [Symbol] -> IO (Bool, [String], [Symbol])
write_stm (w:id:other) st =  do 
                               print (lookup_symtable (filter (/=';') id))
                               return ((True, other, st))

body :: [String] -> [Symbol] -> (Bool, [String], [Symbol])
body [] st = (True, [], st)
body (h:t) st = if h == "begin" then body t st
                else 
                   if h == "end" then body t st
                   else
                      if h == "write" then 
                          let (status, str, sym) = (unsafePerformIO (write_stm (h:t) st)) in
                            if status == True then body str sym
                            else (False, str, sym)
                      else (True, h:t, st)

program :: [String] -> [Symbol] -> (Bool, [String], [Symbol])
program (h0:t0) st = if h0 /= "program" then (False, [errorMsg "program"], st)
                     else 
                       let (s1, r1) = idd t0 in
                         if s1 /= True then (s1, r1, st)
                         else 
                           let (s2, r2, st2) = var_section r1 st in
                             if s2 /= True then (s2, r2, st2)
                             else body r2 st2
                
principal :: String -> (Bool, [String], [Symbol])
principal fn =  program (unsafePerformIO (leia fn)) []

leia fn = do x <- openFile fn ReadMode
             y <- hGetContents x
             return (words y)