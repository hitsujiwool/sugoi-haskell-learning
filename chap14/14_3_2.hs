
import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
   push 3
   pop
   pop
  
-- stackManip :: State Stack Int
-- stackManip = push 3 >>= (\x -> pop >>= (\y -> pop))
  
main :: IO ()
main = do
  print $ runState stackManip [5,8,2,1]
