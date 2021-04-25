import Control.Monad
import Data.List (sort, words, unwords, transpose)
import Data.Set as S (fromList, size)

solucion :: Int -> [[Int]] -> [Int]
solucion n m = [t, r, c]
  where
    -- suma la diagonal
    t = sum $ [xs !! k | (xs, k) <- zip m [0..n]]
    -- repetidos en horizontal
    r = repeatedRows m
    -- repetidos en vertical
    c = repeatedRows $ transpose m
    repeatedRows = length . filter (/= n) . map (S.size . S.fromList)


main :: IO ()
main = do
  -- lee la primera linea y la convierte en un int
  t <- fmap read getLine
  -- replica t veces el bloque, donde t es la cantidad de ejercicios dicha 
  forM_ [1..t] $ \x -> do
    -- lee el tamaño de la matris
    n <- fmap read getLine
    -- lee la matris de n x n de tamaño, poniendo la matris como una lista de listas
    m <- replicateM n $ fmap (map read . words) getLine
    putStr $ "Case #" ++ (show x) ++ ": "
    -- aplica resultado y lo parte para imprimirlo
    putStrLn $ unwords . map show $ solucion n m
