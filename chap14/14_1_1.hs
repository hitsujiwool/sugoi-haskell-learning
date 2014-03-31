
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

main :: IO ()
main = do
    print $ (3, "Smallish gang.") `applyLog` isBigGang
    print $ (30, "A freaking platoon.") `applyLog` isBigGang
    print $ ("Togin", "Got outlow name.") `applyLog` (\x -> (length x, "Applied length."))
    print $ ("Bathcat", "Got outlow name.") `applyLog` (\x -> (length x, "Applied length."))
