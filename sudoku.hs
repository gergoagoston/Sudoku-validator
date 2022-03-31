-- Agoston Gergely Vince 521 agim1986
import System.IO
import Control.Monad
import Data.List

elso x = (take 3 x)
masodik x = (take 3 (drop 3 x))
harmadik x=  (take 3 (drop 6 x))


subs [] = True
subs (x:y:z:xs)
    | (okr ((elso x) ++ (elso y)++ (elso z ))) && (okr ((masodik x) ++ (masodik y)++ (masodik z ))) && (okr ((harmadik x) ++ (harmadik y)++ (harmadik z ))) = subs xs
    | otherwise = False


okr xs = (sort xs) == [1,2,3,4,5,6,7,8,9]
rows [] = True
rows (x:xs)
    | okr x = rows xs
    | otherwise = False

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    
    matrix <-replicateM 9 $ do
        input_line <- getLine
        let input = words input_line
        
        sor <- forM [0..(9-1)] $ \i -> do
            let n = read (input!!(i)) :: Int
            return n
        return sor

    let x = if ((rows matrix) && (rows (transpose matrix)) && (subs matrix))
        then "true"
        else "false"

    
    -- hPutStrLn stderr "Debug messages..."
    
    -- Write answer to stdout
    putStrLn x
    return ()