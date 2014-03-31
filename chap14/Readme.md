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

* 型コンストラクタ `Writer` に `w` のみをを部分適用して、自由引数を1つにした `Writer w` をインスタンス化しているところ (p.154)
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

```
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

```haskell
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

```haskell
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0  = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])    
```

```haskell
finalCountDown :: Int -> Writer [String] ()
finalCountDown 0  = do
    tell ["0"]
finalCountDown x = do
    finalCountDown (x-1)
    tell [show x]
```

ghc の[ドキュメント](http://www.haskell.org/ghc/docs/latest/html/users_guide/profiling.html)によれば、`-prof` オプションをつけてコンパイルした後、`+RTS -p` オプジョンで実行するとプロファイルが見られるそう。

## 計算の状態の正体

状態が問題になるような計算と、純粋関数型言語としての Haskell の橋渡しをするのが `State` モナド。

### 状態付きの計算

状態付きの計算をモデル化する。

```
s -> (a, s)
```
ここで、`s` は状態の型、`a` は状態付き計算の結果、という風に考える。するとこれもまた、これまで繰り返し取り扱ってきた「文脈」と同じ性質のものだとみえてくる。

### スタックと石

例としてスタックのモデル化を考える。スタックは `push` と `pop` の操作を受けつける、後入れ先出しのデータ構造。ここでは2つの操作が、操作後の状態であるところのスタックを、計算の結果と一緒に返すようにする。

```haskell
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stac -> ((), Stack)
push a xs = ((), a:xs)
```

スタックを実際に操作する関数 `stackManip` も用意する

```haskell
stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
  in pop newStack2
```

```
ghci> stackManip [5,8,2,1]
```

これって結局はモナドなのでは？　なので

```haskell
stackManip = do
    push 3
    a <- pop
    pop
```

のように簡潔に書けるとうれしい。見た目をきれいだし、なにより「結果の値と現在の状態を取り出して、次の操作に渡す部分、すなわち複数の状態付き計算の糊付けを隠蔽できる」ところが重要。

ということで `State` モナドの導入。

### State モナド

```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

```haskell
instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s
                                        (State g) = f a
                                    in g newState
```

* `return` は、`State` モナドを返す。

次に `>>=` について。これは2つの状態付き計算の糊付けを行う。

まず全体を眺めると、次のようなことがわかる。

1. `>>=` は `State` モナドを返す
2. パターンマッチで `State` モナドの中の関数 `s -> (a, s)` を `h` に束縛している
3. そのモナドは、`runState` で 「初期状態を渡したときに、何らかのタプルを返すような関数」を返す

ではその関数はどんなふるまいをするのかというと

1. ユーザが `runState` を実行したときに渡される現在の「状態」`s` に `h` を適用させ、得られた値と新しい「状態」をそれぞれ `a` と `newState` に束縛する
2. `f` を `a` に適用し、返り値の `State` モナド、すなわち「状態付き計算」（の中の関数）を `g` に束縛する
3. `g` を `newState` に適用し、得られたタプルを返す

よくわからない……とりあえず `State` モナドを使ったスタックの再実装を見てみる。

```haskell
pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  pop
  pop
```

`state` 関数を使って `State s a` 型を返すようにしているのがポイント。

do 記法なしで書くと（こちらの方が `>>=` が見えるので理解の手助けになるかも）

``` haskell
stackManip :: State Stack Int
stackManip = push 3 >>= (\x -> pop >>= (\y -> pop))
```

```
ghci> runState stackManip [5,8,2,1]
```

基本的な構造はこれまで扱ってきたさまざまなモナドと同様である。にもかかわらず、わかったようなわからないような……の理由はたぶん

* `runState` は単なるデータのアクセサではなく、引数に初期状態を与えて計算を実行する関数
* `State` モナドは「状態」ではなく「状態付き計算」

整理のために、以下を実行したときの挙動を追ってみる（とりあえず遅延評価の細かい仕組みは考えない）。

```haskell
stackManip :: State Stack Int
stackManip = pop >>= (\x -> pop)
```

```
ghci> runState stackManip [5,8,2,1]
```

上述の、`runState` に初期状態を与えて実行した関数のふるまいに対応した形で書くと

1. `pop` が返す `State` モナドの中の関数を実行することで得られたタプル `(5, [8,2,1])` の要素をそれぞれ `a` と `newState` に束縛する
2. `(\x -> pop)` に `5` を適用し、返り値の `State` モナドの中の関数を `g` に束縛する
3. `g` を `[8,2,1]` に適用し、返り値のタプル `(8, [2,1])` を返す

確かに、タプルを分解したり、次に渡したりする処理がうまく隠蔽されている。

### 状態の取得と設定

`get` や `put` を使うと現在の状態を取得したり、書き換えたりできる。

### 乱数と State モナド

さて、`State` モナドと使い方がわかると、節の冒頭で紹介された乱数生成の関数をもっとシンプルに書けるようになる。

```haskell
random :: (RandomGen g, Random a) => g -> (a, g)
```

乱数ジェネレータを引数にとり、乱数と新しいジェネレータの組を返すという構造はスタックの例と同じ。

```haskell
import System.Random
import Control.Monad.State

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a, b, c)
```
