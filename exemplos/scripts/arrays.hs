import Array

{- Cria��o de um array com �ndices de 1 a 10, 
   onde a posi��o i armazena o valor i*i. -}
v1 = array (1,10) [(i,i*i) | i <- [1..10]]

{- v2 ser� o vetor v1 ap�s atribuir o valor
   555 � posi��o de �ndice 5. -}
v2 = v1 // [(5,555)]

{- n ser� o elemento armazenado no �ndice 1 de v2 -}
n = v2!1