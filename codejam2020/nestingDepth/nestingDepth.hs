import Control.Monad
import Data.Text(unpack,pack,replace)

invalidSolucion :: [Char] -> [Char]
invalidSolucion (x:xs) = encerrarNumeros x ++ invalidSolucion xs
invalidSolucion x = ""


encerrarNumeros :: Char -> [Char]
encerrarNumeros x = (replicate (read [x]) '(' ) ++ [x] ++ (replicate (read [x]) ')' )

removeInvalid :: String -> String
removeInvalid s =
  let
      t = unpack . replace (pack ")(") (pack "") $pack  s
  in
    if s == t then s else removeInvalid t

main :: IO ()
main = do
  -- lee la primera linea y la convierte en un int
  t <- fmap read getLine
  -- replica t veces el bloque, donde t es la cantidad de ejercicios dicha
  forM_ [1..t] $ \x -> do 
    n <- getLine
    putStr $ "Case #" ++ (show x) ++ ": "
    -- pasa la lista de numeros como char a encerrar en parentesis, donde encierra 
    -- los numeros en esa cantidad de panrentesis y luego le saca 
    -- los parentesis sobrantes 
    putStrLn $removeInvalid $invalidSolucion n