---
title: "素数判定ふたたび！練習問題(12問目)【Haskellで競技プログラミングがしたい！ Part6】"
emoji: "👻"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["Haskell", "Codespaces", "Atcoder", "アルゴリズム×数学"]
published: true
---
## はじめに

最近は研究したいテーマが多く、なかなか手を付けることができませんでした。
主にNostrに興味があり、プロトコルをもとに実装すれば自由にSNSを扱うことができるという面白さがありその周辺技術の理解を進めたり、GolangやJavascriptで簡単なアプリを動かしながら動作を確かめています。
ただこのHaskellで問題を解くこと自体は楽しいので、1問でもいいからコツコツ続けようということでやっていきます
今回は12問目について回答してみましょう。
https://atcoder.jp/contests/math-and-algorithm/tasks

## 12問目

### 問題

N が素数であるかどうかを判定してください。

### 解答

```haskell
isPrime :: Int -> [Int] -> String
isPrime _ [] = "Yes"
isPrime n (x:xs) = if isDivisible n x
    then "No"
    else isPrime n xs

sqrtInt :: Int -> Int
sqrtInt n = floor $ sqrt (fromIntegral n)

isDivisible :: Int -> Int -> Bool
isDivisible x y = x `mod` y == 0

main :: IO ()
main = do
    n <- readLn
    putStrLn $ isPrime n [2..sqrtInt n]
```

### 解説

前回の素数判定部分さえ書けばOKの問題です。
ただせっかくなのですこし違うアプローチを試してみました。
`isPrime`は引数に**Int**と[**Int**]型を取り、リストをパターンマッチングで先頭とそれ以降に分けます。
`n`と先頭の値で`isDivisible`をして`True`が返ってきた時点で素数ではないので`No`を表示し、そうでなければ残りのリストを引数にとった`isPrime`を再帰で実行します。
引数のリストが空になったとき`isPrime`は`Yes`を返すので、これによって素数が判定できます。

## 終わりに

Webフロントエンドで使われるElmも並行して使っているので、共通している部分もあり引き続き練習していきたいと思います！
見ていただきありがとうございました！
