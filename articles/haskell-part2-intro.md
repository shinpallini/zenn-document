---
title: "Haskellで競技プログラミングがしたい！Part2 練習問題を解く(1~3問目)"
emoji: "📑"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["Haskell", "Codespaces", "Atcoder", "アルゴリズム×数学"]
published: true
---
## はじめに
以下のリンクに、アルゴリズム×数学の練習問題があります。
今回はまず1~3問目について回答してみましょう。
https://atcoder.jp/contests/math-and-algorithm/tasks

## 1問目

### 問題
りんごが 5 個あり、みかんが N 個あります。
整数 N が与えられるので、りんごとみかんを合わせて何個あるかを出力するプログラムを作成してください。

### 解答
```haskell
main = do
    input <- getLine
    let n = read input :: Int
    putStrLn $ show (n + 5)
```

### 解説
基本的な処理ですが、今後も使うであろう標準入力や標準出力周りの操作を確認します。
- `getLine`
    - 標準入力から1行読み取り、**IO String** 型を返す
    - `<-` を使って変数に**String** をBindする
- `read`
    - **String** 型を受け取って指定の型へ変換する
    - 今回は数値計算なので**Int** 型へ変換する
- `show`
    - **Show**型クラスのインスタンスである任意の型**a** を受け取り、**String** 型の値を返す
    - インスタンスは別途学習する。
        - ざっくり把握すると型クラスのことで、型がどのような役割を持つかについてのカテゴリのこと
        - ほかのインスタンスには**Eq**(`==`の比較ができる)などがある
        - わかりやすい説明があったページがあった[1]
    - `ghci`で確認すると以下のような関数ということがわかる
    ```haskell
    ghci> :t show
    show :: Show a => a -> String
    ```
- `putStrLn`
    - **String**型を引数にとって、その文字列を標準出力へ書き込む

上記のものを抑えておけば基本的な入力と出力は扱えそうです。
今後また必要な時に新しい関数が出たときは追加でまとめていきます。

## 2問目
### 問題
3 つの整数 
$${A_1,A_2,A_3}$$が与えられます。$${A_1+A_2+A_3}$$を出力してください
### 解答
```haskell
solve :: [Int] -> Int
solve list = foldr1 (+) list

main = do
    input <- getLine
    let list = map read $ words input :: [Int]
    putStrLn $ show $ solve list
```
### 解説
今回は標準入力からスペース区切りの文字列が入ってくるので、それをリストとして受け取ります。
そのために`map`を使用してリストのすべての要素に関数を適用します。
今回適用するのは先ほど利用した`read`で、これにより[**String**]を[**Int**] にして`list`に代入できます。
そのほかの関数については以下に記載します。
- `foldr1`
    - リストを特定の関数を引数にとり1つの値に畳み込む関数
    - 今回は`(+)`を引数にとって、リストの要素を1つずつ足しこんでいく
    - よく使うのは`foldr`だが、`foldr1`と1がつくことでリストの右端の値を初期値として使える
    - `foldl`も同様
    - 説明はこのサイトがとても分かりやすかった[2]

## 3問目

### 問題
整数N とN 個の整数 
$${A_1, A_2, ⋯, A_N}$$が与えられます。（入力の形式は「入力」セクションを参照）

$${A_1+A_2+⋯+A_N}$$を出力してください。
### 解答
```haskell
solve :: [Int] -> Int
solve list = foldr1 (+) list

main = do
    _ <- getLine
    input <- getLine
    let list = map read $ words input :: [Int]
    putStrLn $ show $ solve list
```
### 解説
ほぼ2問目と同じ構成です。
違うのは標準入力の1行目に要素数を表すNがあるのですが、この実装では使わないので`_`へBindしています。

## 終わりに
実装自体はシンプルでしたが、競技プログラミングをやっていく上での基本的な標準入力・標準出力の扱い方が理解できました。
引き続き次の課題についても進めていこうと思います。
見ていただきありがとうございました！

## 参考
1. インスタンス周りの話で出てきた型クラスについて

http://walk.northcol.org/haskell/type-classes/

2. foldl, foldrのわかりやすい説明を解説してくれている

https://taiju.hatenablog.com/entry/20130202/1359773888
