---
title: "Haskellで競技プログラミングがしたい！Part4 練習問題を解く(7~9問目)"
emoji: "📌"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["Haskell", "Codespaces", "Atcoder", "アルゴリズム×数学"]
published: false
---

## はじめに
少しずつ進んできましたね。この調子で頑張ります！
さて今回は7~9問目について回答してみましょう。
https://atcoder.jp/contests/math-and-algorithm/tasks

## 7問目
### 問題
N 以下の正の整数の中で、X の倍数または Y の倍数であるものの個数はいくつありますか？

### 解答
```haskell
isMultiple :: Int -> Int -> Int -> Int
isMultiple x y n
    | n `mod` x == 0 || n `mod` y == 0 = 1
    | otherwise = 0

solve :: Int -> Int -> Int -> Int
solve x y n = sum $ map (isMultiple x y) [1..n]

main:: IO ()
main = do
    input <- getLine
    let [n, x, y] = map read (words input) :: [Int]
    print $ solve x y n
```
### 解説
どのように計算するか迷ったのですが、今回思いついたのはガード条件を使った倍数判定です。
これを使うと関数のパターンマッチングにおいて条件を指定することができます。
`isMultiple`は、今回だと`x`または`y`で割ったときのあまりが0の時に1を返し、それ以外の時は0を返すように実装しました。
次にこの関数を使って`solve`を実装します。
`1`から`n`までに対して`isMultiple`を適用して`1`になったものの合計をとると個数が数えられそうと思いつき、
- `map`で` [1..n] `までの値に`isMultiple`を適用する
- この処理の返り値のリストに`sum`して計算

としました。
最後に標準入力ですが、変数に値をBindするときもパターンマッチングが使えます。
なので入力が3個であることが事前にわかっているので、` [n, x, y] `として値を受け取れました。

## 8問目
### 問題
赤・青のカードが各1 枚ずつあり、あなたはそれぞれのカードに 1 以上 N 以下の整数を 1 つ書き込みます。
カードに書かれた整数の合計が S 以下となる書き方は、いくつありますか？
### 解答
```haskell
solve :: Int -> Int -> Int
solve n s = sum [1 | i <- [1..n], j <- [1..n], i + j <= s]

main:: IO ()
main = do
    input <- getLine
    let [n, s] = map read (words input) :: [Int]
    print $ solve n s
```
### 解説
総当たりで計算したかったのでリスト内包表記を使いました。
内包表記とはもともと数学の集合に関する仕組みで、既存の集合から新たな集合を作るために使われるそうです。[1]
今回は2種類のカードがあり、それぞれ最大`n`までの値をとるので、それぞれのカードがとる数字の集合、つまりリストは` [1..n] `で表現することができます。
またこの内包表記には条件を設定することができ、この条件が`True`になったものだけ新しいリストに追加されます。
今回は`s`以下が条件なので、2種類のカードの数字の合計`i+j`が`s`以下なら`True`となりリストに`1`が追加されます。
最後にこの処理によって新しいリストが生成されるので、`sum`して組み合わせの数を合計しています。

標準入力は前のものと同様にパターンマッチングで変数にBindしました。

## 9問目

### 問題文にあった注意書き
全探索で解いても 1000 点中 500 点しか得られず、満点（AC）にならないことに注意してください。（本に記されている通り、一部の大きいケースでは現実的な時間で答えが求まらないからです）

### 問題
N 枚のカードが横一列に並べられています。左からi 番目 
$${{(1≤i≤N)}}$$ のカードには整数 $${{A_i}}$$が書かれています。
カードの中からいくつかを選んで、合計がちょうど S となるようにする方法はありますか。
### 解答
```haskell
intToBits :: Int -> [Bool]
intToBits 0 = []
intToBits n = (n `mod` 2 == 1) : intToBits (n `div` 2)

genBits :: Int -> [[Bool]]
genBits n = [intToBits i | i <- [1..2^n-1]]

selectedSum :: [Int] -> [Bool] -> Int
selectedSum nums bits = sum $ zipWith (\b x -> if b then x else 0) bits nums

check :: Int -> [Int] -> [Bool] -> Bool
check s nums bits = s == selectedSum nums bits

output :: Bool -> String
output bool = if bool then "Yes" else "No"

main:: IO ()
main = do
    input <- getLine
    let [n, s] = map read (words input) :: [Int]
    input <- getLine
    let nums = map read (words input) :: [Int]
    let allBits = genBits n
    putStrLn $ output $ any (==True) $ map (check s nums) allBits
```
### 解説

## 終わりに

## 参考
内包表記で参考になった記事

https://zenn.dev/masahiro_toba/articles/67c1b3c9e3b809