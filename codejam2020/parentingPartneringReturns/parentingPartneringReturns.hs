import Control.Monad
import Debug.Trace


puedeAgregar :: (Int,Int) -> [(Int,Int)] -> Bool 
puedeAgregar x [] = True
puedeAgregar x (y:ys)
  | fst x > fst y && fst x < snd y = False
  | snd x > fst y && snd x < snd y = False 
  | otherwise = puedeAgregar x ys


agregar :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
agregar x [] = [x]
agregar x (y:ys)
  | fst x > fst y && fst x >= snd y = x: y: ys
  | otherwise = y : agregar x ys


solucion :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> String -> String
solucion [] _ _ respuesta = respuesta
solucion (x:xs) c j respuesta
  | puedeAgregar x c = solucion xs (agregar x c) j respuesta ++ "c"
  | puedeAgregar x j = solucion xs c (agregar x j) respuesta ++ "j"
  | otherwise = "IMPOSSIBLE"

listToTuples :: [String] -> (Int,Int)
listToTuples x = (read $head x,read $last x)

main :: IO ()
main = do
  -- lee la primera linea y la convierte en un int
  t <- fmap read getLine  
  -- replica t veces el bloque, donde t es la cantidad de ejercicios dicha
  forM_ [1..t] $ \x -> do
    -- lee la linea donde dice la cantidad de horarios
    n <- fmap read getLine
    -- lee los n cantidad de horarios y los pone en una lista como tuplas
    -- dando una lista de tuplas
    m <- replicateM n $ fmap (listToTuples . words) getLine :: IO [(Int,Int)]
    putStr $ "Case #" ++ show x ++ ": "
    -- aplica solucion y la imprime
    print $solucion m [] [] ""