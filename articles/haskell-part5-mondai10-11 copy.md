---
title: "素数判定を実装する！練習問題(10~11問目)【Haskellで競技プログラミングがしたい！ Part5】"
emoji: "📝"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["Haskell", "Codespaces", "Atcoder", "アルゴリズム×数学"]
published: true
---
## はじめに
今日も少し進めていきます！
さて今回は10~11問目について回答してみましょう。
https://atcoder.jp/contests/math-and-algorithm/tasks

## 10問目
### 問題
N! の値を求めてください。
### 解答
```haskell
fact :: Int -> Int
fact 1 = 1
fact n = n * fact(n-1)

main :: IO ()
main = do
    n <- readLn
    print $ fact n
```
### 解説
関数をシンプルなパターンマッチングで実装することで階乗が計算できます。

## 11問目
### 問題
N 以下の素数を、小さい順に出力してください。
### 解答
```haskell
solve :: Int -> [Int]
solve n = filter isPrime [2..n]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (isNotDivisible n) [2..end]
    where 
        end = floor $ sqrt (fromIntegral n)

isNotDivisible :: Int -> Int -> Bool
isNotDivisible x y = x `mod` y /= 0

main :: IO ()
main = do
    n <- readLn
    putStrLn $ unwords $ map show (solve n)
```
### 解説
今回の素数判定法は、入力`n`に対して$${\sqrt{n}}$$以下まで調べて割り切れなければ素数と言ってよい、という手法を使います。
まずは割り切れるかどうかについて判定する関数の`isNotDivisible`を用意し、それを使って`isPrime`を実装します。
```haskell
isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (isNotDivisible n) [2..end]
    where 
        end = floor $ sqrt (fromIntegral n)
```
リストを2から`end`の値まで用意し、そのすべてで割り切ることができなかった場合に`True`を返し、そうでない場合は`False`を返します。

最後に`filter`を使って`isPrime`が`True`になるものだけ抽出して出力することで完了です。

## 終わりに
短くなりましたが本日はここで終わろうと思います。
少しずつ考えられることが増えてきたのでこの調子で1つずつできることを増やしていきたいです。
見ていただきありがとうございました！
