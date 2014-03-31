# 第14章　もうちょっとだけモナド

## Writer？　中の人なんていません

まずはじめに「ログを追加する」という機能について考えてみる。ログを追加するとはすなわち、ある関数に文脈のついた値を食べさせて、文脈に新しい情報を追加することなのだから、前章で学んだ `applyMaybe` によく似た `applyLog` を作れそう（まだ出発点なので `>>=` は使わない）。

```haskell
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)
```

値を食べさせる関数も定義する

```haskell
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")
```

試してみると

```
ghci> (3, "Smallish gang.") `applyLog` isBigGang
ghci> (30, "A freaking platoon.") `applyLog` isBigGang
ghci> ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied lengh."))
ghci> ("Bathcat", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied lengh."))
```

### モノイドが助けにきたよ

さらに「ログを追加する」というふるまいをより一般化して考えてみる。

1. ログの追加とは、各々の結果であるところの文字列を `++` で連結することである
2. `++` で追加できるなら任意の型のリストでもよくない？
3. そもそも追加というふるまいをもう少し一般化できるよね？
4. あ、それってモノイドと `mappend` なのでは？

```haskell
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)
```

対象を「値と、ログの組」ではなく「値と、モノイドのおまけ」とみなすことで、`applyLog` により一般的な機能を持たせられるようになる。たとえば食べ物の注文を受け取って、それに合う飲み物と注文の金額をタプルにして返す `addDrink` を考える。`applyLog`はそのまま。

```haskell
import Data.Monoid

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)
```

おまけの結果を `Sum a` 型のモノイドにしている点に注意。

```
ghci> ("beans", Sum 10) `applyLog` addDrink
ghci> ("jerky", Sum 25) `applyLog` addDrink
ghci> ("dogmeat", Sum 5) `applyLog` addDrink
```

連結もできる。

```
ghci> ("dobmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink
```

### Writer 型

さて、このようなモノイドのおまけの付いた値」はいかにもモナドっぽいし、実際に `Control.Monad.Writer` というモジュールが用意されている。

`Write w a` 型はタプルの `newtype` として定義されており

```haskell
newtype Writer w a = Writer { runWriter :: (a, w) }
```

ここで `a: 主となる値の型` 、また `w: おまけのモノイド値の型` である。p.260 の `Pair b a` 型の例と同様に型引数の順番を入れ換えて、`f` で変換される方の `a` を2番目に持ってきている点に注意。

この `Writer` がどのようにモナドを実装しているかというと……

```haskell
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x
                             in Writer (y, v `mappend` v')
```

読解のポイントは

* 型コンストラクタ `Writer` に`w`のみをを部分適用して、自由引数を1つにした `Writer w` をインスタンス化しているところ (p.154)
* パターンマッチで取り出した `x` を `f` に適用した結果の `Writer` を、さらに `let` 内の'パターンマッチで `y` と `v'` に束縛しているところ

Monadの定義 (p.286) に従っていることも見てとれる。

```haskell
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```

おまけの値の型（どれもモノイド）をいろいろ試してみる

```
ghci> runWriter (return 3 :: Writer String Int)
ghci> runWriter (return 3 :: Writer (Sum Int) Int)
ghci> runWriter (return 3 :: Writer (Product Int) Int)
```

### Writer を do 記法で使う

モナドといえば do 記法。文脈を気にせず中の値を透過的に操作できる。

```haskell
import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)
```

do 記法なしの `>>=` と入れ子になったラムダ式 (p.296) でも書ける。

```haskell
multWithLog :: Writer [String] Int
multWithLog = do
    logNumber 3 >>= (\x -> logNumber 5 >>= \y -> return (x*y))
```
```
ghci> runWriter multWithlog
```

モノイド値だけ追記する場合は `tell` を使う。

```
multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)
```

### プログラムにログを追加しよう

`Writer` を使ってユークリッドの互除法にログをつけてみる。まずは普通のログなしバージョン

```haskell
gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)
```

続いてログありバージョン

```haskell
import Control.Monad.Writer

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)
```

変更点は

* `gcd` の返り値を `Writer [String] Int` 型に変更する
* `tell` を使って途中経過を記録する
* `do` ないし `>>=` を使って関数適用をモナドスタイルにする

`mapM_` (p.172) を使ってログを出力してみると

```
ghci> fst $ runWriter (gcd' 8 3)
ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
```

```
8 mod 3 = 2
3 mod 2 = 1
2 mod 1 = 0
Finished with 1
```

### 非効率なリスト構築

ログのデータ構造が左結合になっていると効率が悪いよ、というお話。p.8 にあったように、長いリストの末尾にリストを連結する処理は、長いリストの先頭にリストを追加するより時間がかかる。

たとえば下記の `gcdReverse` はデータの格納順序が `gcd` の場合と逆になっている。したがって、`mapM_` したときの順序もさかさま。

```haskell
import Control.Monad.Writer

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a      
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result
```        

```haskell
ghci> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)
```

```
Finished with 1
2 mod 1 = 0
3 mod 2 = 1
8 mod 3 = 2
```

右結合の `gcd'`（効率的）

```
a ++ (b ++ (c ++ (d ++ (e ++ f))))
["8 mod 3 = 2"] ++ (["3 mod 2 = 1"] ++ (["2 mod 1 = 0"] ++ ["Finished with 1"]))
```

左結合の `gcdReverse`（非効率的）

```
((((a ++ b) ++ c) ++ d) ++ e) ++ f))))
((["Finished with 1"] + ["2 mod 1 = 0"]) ++ ["3 mod 2 = 1"])) ++ ["8 mod 3 = 2"]
```

### 差分リストを使う

差分リストはデータ構造の形をした関数のこと。`[1,2,3]`というリストと等価な差分リストは `\xs -> [1,2,3] ++ xs]` つまり `([1,2,3]++)`

* `toDiffList` は元のリストを、そのリストを別のリストの先頭に結合するような関数に変換する
* `fromDiffList` は `DiffList a`の中身に空リストを作用させてリストを返す
* `mempty` は自身を返す `id`、`mappend` は関数合成import Control.Monad.Writer

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))
```

というわけで `gcdReverse` を書き換えてみると

```haskell
gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a      
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result
```

ここで `result` にバインドする場所は `gcdReverse` と変わっていないことに注意する。つまり出力結果は `gcdReverse` と同様に `Finished with 1` で始まるし、`mappend` が左結合という順序も同じ。ただ、結果として合成された関数が右結合で、リストの先頭に結合できるようになっている、というのが直感的な説明か？

```
(((a . b) . c) . d) . e
a (b (c (d (e xs))))
```

実行すると

```
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34
```
```
Finished with 2
8 mod 2 = 0
34 mod 8 = 2
110 mod 34 = 8
```

### 性能の比較

`DiffList` を使う場合とそうでない場合の性能を比較してみる。

```
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0  = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])    
```
```
finalCountDown :: Int -> Writer [String] ()
finalCountDown 0  = do
    tell ["0"]
finalCountDown x = do
    finalCountDown (x-1)
    tell [show x]
```

ghc の[ドキュメント](http://www.haskell.org/ghc/docs/latest/html/users_guide/profiling.html)によれば、`-prof` オプションをつけてコンパイルした後、`+RTS -p` オプジョンで実行するとプロファイルが見られるそう。
