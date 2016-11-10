import Array

{- Criação de um array com índices de 1 a 10, 
   onde a posição i armazena o valor i*i. -}
v1 = array (1,10) [(i,i*i) | i <- [1..10]]

{- v2 será o vetor v1 após atribuir o valor
   555 à posição de índice 5. -}
v2 = v1 // [(5,555)]

{- n será o elemento armazenado no índice 1 de v2 -}
n = v2!1