---
title: "Haskellで競技プログラミングがしたい！Part3 練習問題を解く(4~6問目)"
emoji: "📌"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["Haskell", "Codespaces", "Atcoder", "アルゴリズム×数学"]
published: false
---

## はじめに
以下のリンクに、アルゴリズム×数学の練習問題があります。
今回は4~6問目について回答してみましょう。
https://atcoder.jp/contests/math-and-algorithm/tasks

## 4問目
### 問題
3 つの整数 
$${A_1,A_2,A_3}$$が与えられます。
$${A_1A_2A_3}$$を出力するプログラムを作成してください。
### 解答
```haskell
solve :: [Int] -> Int
solve list = foldr1 (*) list

main = do
    input <- getLine
    let list = map read $ words input :: [Int]
    putStrLn $ show $ solve list
```
### 解説
前回の記事で書いた2,3問目とほぼ一緒になります。
違う点は`foldr1`で適用したい関数を`(+)`から`(*)`に変えたところのみです。

また、前回解説できていなかった要素に`$`があります。[1]
これは**関数適用演算子**と呼ばれコードを読みやすく書くための便利な演算子です。

例えば今までのケースだと`putStrLn`で出力したいものは**Int**型なので`show`を使いたいです。
簡単のためこれを以下のようにそのままつなげて実行すると以下のようなエラーになります。
```haskell
ghci> putStrLn show 3

<interactive>:20:1: error:
    • Couldn't match expected type: t0 -> t
                  with actual type: IO ()
    • The function ‘putStrLn’ is applied to two value arguments,
        but its type ‘String -> IO ()’ has only one
      In the expression: putStrLn show 3
      In an equation for ‘it’: it = putStrLn show 3
    • Relevant bindings include it :: t (bound at <interactive>:20:1)

<interactive>:20:10: error:
    • Couldn't match type: a0 -> String
                     with: [Char]
      Expected: String
        Actual: a0 -> String
    • Probable cause: ‘show’ is applied to too few arguments
      In the first argument of ‘putStrLn’, namely ‘show’
      In the expression: putStrLn show 3
      In an equation for ‘it’: it = putStrLn show 3
```
要約すると、
- 関数の引数が不適切
    - `putStrLn` は一つの**String** 型の引数を取る関数ですが、ここでは二つの値（`show` と `3`）が引数として渡されている。これは`putStrLn`の型 **String** -> **IO ()** と一致しないためエラーが発生している。
- `show` 関数の不適切な使用
    - `show` は引数を一つ取る関数だが、このコードでは引数なしで使用されている。そのため、`show` は **a0** -> **String** 型（任意の型の値を受け取って **String** を返す関数）と推論されているが、**putStrLn** には **String** 型の引数が必要になる。

つまり`putStrLn`が2つの引数をとる関数のように扱われてしまっており、型が一致しないのでエラーになっています。。
本来は先に`show 3`して、その結果を`putStrLn`したいので、この計算順序を実現するために`()`または`$`が使われます。
`$`を使える利点としては、関数が入り組んで適用されているときにかっこが深くなりにくくシンプルに記述しやすくなることです。

## 5問目
### 問題
N 個の整数$${a_1,a_2,...,a_n}$$が与えられます。
$${(a_1+a_2+...+a_n)\ mod\ 100}$$の値を出力してください。
### 解答
```haskell
solve :: [Int] -> Int
solve list = foldr1 (+) list `mod` 100

main = do
    _ <- getLine
    input <- getLine
    let list = map read $ words input :: [Int]
    putStrLn $ show $ solve list
```
### 解説
今回は`` `mod` ``という関数が出てきました。
`` `mod` ``は標準で用意されている関数で、余りを計算できます。
```haskell
ghci> :t mod
mod :: Integral a => a -> a -> a
```
2個の**Integral**型である値を引数にとって余りを計算してくれます。
また、関数なので後ろに引数を置く形が一般的ですが2個の引数をとる関数にのみバッククォートで囲むことで中置演算子のように扱うことができます。
今回`solve`関数では配列のすべての値を足しこんだものと`100`の間に`` `mod` ``を置くことで演算子のように扱って余りを計算できます。
加えてこの`` `mod` ``が間にいることによって`foldl`の処理をかっこで囲まなくてよいのも楽でよいですね。
例えば`mod`を前に置いた場合は以下のような記述になります。
```haskell
mod (foldr1 (+) list) 100
```

## 6問目
### 問題
整数 N が与えられます。2N+3 の値を出力してください。
### 解答
```haskell
main = do
    input <- readLn
    putStrLn $ show (input * 2 + 3)
```
### 解説
シンプルな計算なので3行で書けました。
そしてシンプルに書くために今回標準入力の受け取りも今までの`getLine`から`readLn`を使用してみました。[2]
これは入力の型を推論して読み込めます。
つまり`readLn`によって読み込んだ変数を使って足し算などをすると**Int**や**Double**などであると推測してくれるので便利です。
実際は`read`で読み込んだ変数を使うとこちらも同様に型を明示しなくても推論で計算してくれるので型推論の部分に関してはほぼ変わりませんが、`readLn`を使うと`getLine`1行分省略できますね。
```haskell
main = do
    -- input <- readLn
    input <- getLine
    let n = read input
    putStrLn $ show (n * 2 + 3)
```
## 終わりに
最初なのでサクサク進んでいきますね！
問題が進むごとに少しずつ難しくなっていくと思うので、その問題に合わせて調べつつ理解していこうと思います。
読んでいただきありがとうございました！

## 参考
1. 関数適用演算子は右結合だが、その結合について解説してくれている

https://haskell.hatenablog.com/entry/Haskell-right-associativity-and-left-associativity

2. その他競技プログラミングで使う標準入力の処理について具体的に書かれている。パターンマッチングも使えると便利そう

https://qiita.com/karszawa/items/ec0c01c81c22ce060405