
import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)

-- multWithLog :: Writer [String] Int
-- multWithLog = logNumber 3 >>= (\x -> logNumber 5 >>= \y -> return (x*y))

main :: IO ()
main = do
    print $ runWriter multWithLog
