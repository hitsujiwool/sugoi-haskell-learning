
import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = do
    (x:xs) <- get
    put xs
    return x

push :: Int -> State Stack ()
push x = do
    xs <- get
    put (x:xs)        

stackManip :: State Stack Int
stackManip = do
   push 3
   pop
   pop

main :: IO ()
main = do
  print $ runState stackManip [5,8,2,1]
