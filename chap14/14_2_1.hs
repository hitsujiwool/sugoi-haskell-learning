
import Control.Monad.Instances

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

-- addStuff :: Int -> Int
-- addStuff = (*2) >>= (\x -> (+10) >>= (\y -> return (x+y)))

main :: IO ()
main = do
    print $ addStuff 3
