---
title: "GolangでシンプルなIOモナドを実装してみよう"
emoji: "⛳"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["Haskell", "Monad", "モナド", "Golang", "Go"]
published: false
---

# はじめに

Haskellなどの純粋関数型言語を勉強している中で出てくる「**モナド**」があります。
この概念は最初よくわからず、色々調べる中で色々な表現で説明されていました。
例えば

- プログラムを構造化するための汎用的な抽象概念 [\[1\]](#参考)
- 自己関手の圏におけるモノイド対象 [\[2\]](#参考)
- `bind`関数と`toMonad`関数を持つ型 [\[3\]](#参考)

のようにいくつかの表現がありました。
調べている中で一番しっくり来たのが以下の記事(参考3)だったので、これをベースにシンプルなIOモナドを実装していきます。

https://www.infoq.com/jp/articles/Understanding-Monads-guide-for-perplexed/

この記事では基本的な標準入力を取得する関数`nextInput()`から始まり、これがシステムの状態に依存する変数であること、そして最終的にはモナドを活用してコード内にシステムの状態を明示的に表す部分を取り除くことに成功しています。
この流れに沿って実際に必要な関数をGolangで実装していきましょう。

なぜGolangか、というのは単純に自分が書きやすいという理由です。

# 実装方針

今回実装する必要がある関数は以下になります。

- `doInput`
    - 標準入力から1行読み取って文字列として取得する関数で、**アクション**を返す
- `doPrint`
    - 文字列を引数にとって標準出力に出す関数で、**アクション**を返す
- `bind`
    - 2つのアクションを1つにつなげる関数で、**アクション**を返す

これらの関数はすべてアクションを返す関数であり、つまりすべて同じコンテキストであると言えます。
また、`doInput()`が返すのは**文字列**ではなく**アクション**であり、この返り値自体を文字列操作するような方法はできません。
例えば以下のようなコードを書いたとします。

```go
func main() {
    input := doInput()
    fmt.Println("input: " + input)
}
```

あくまでこの関数が返すのは「**標準入力から1行取得するというアクション**」なので、文字列操作はできません。
そしてこれを解決するために文字列を変数に代入してしまうと、状態を持った変数がプログラム上に現れてしまいます。

これを解決するために、`doPrint`という「**標準出力に文字列を出すアクション**」を`bind`で連結させて、1つのアクションとして実行させることを考えます。
もう少し具体的に言うと、関数`bind`の目的は、「**任意のアクションAと任意の文字列からアクションへの関数fを受け取り、それらを組み合わせて新しいアクションを作る**」ことになります。
今回はIOアクションなので、`doPrint`実行後は、Haskellでいう`IO ()`のような状態であってほしいです。
これをどう表現するべきかが難しかったのですが、今回は`doPrint`は空文字を含んだアクションを返すことにしています。

これにより一連のコンテキストの中で値を受け渡すことで外界に状態を公開することなく処理をすることができます。

では実際に具体的な実装について見ていきましょう。

# 実装

以下にそれぞれの要素をどのように実装したかについて解説していきます。


- **`func() string`の型である`IOMonad`型を定義する**

```go
type IOMonad func() string
```

`IOMonad`というコンテキストで処理するために関数でラップしています。
これにより標準入力から取得する情報が関数の外部に露出しません。
同様に標準出力する際も関数でラップすることによって副作用を隠蔽しています。

- **`doInput`で1行の文字列を読み込む関数を定義**

```go
func doInput() IOMonad {
	return func() string {
		sc := bufio.NewScanner(os.Stdin)
		sc.Scan()
		return sc.Text()
	}
}
```

この関数は`IOMonad`型を返します。
具体的には`*bufio.Scanner`を使って1行読み取り、その読み取った文字列を返す関数(`IOMonad`)を返します。

- **`doPrint`で引数の文字列を出力する関数を定義**

```go
func doPrint(s string) IOMonad {
	return func() string {
		fmt.Println(s)
		return ""
	}
}
```

この関数も`IOMonad`型を返します
引数に取った文字列をシンプルに`fmt.Println()`で標準出力に出しています。
そして実装方針でも話した通り、空文字を返す関数(`IOMonad`)を返します。

- **`bind`で2つのアクションを連結させる**

```go
func bind(m IOMonad, f func(string) IOMonad) IOMonad {
	input := m()
	result := f(input)
	return result
}
```

この関数の内部では、第1引数に取った`IOMonad`を関数として実行します。
これにより`doInput`のコンテキストの中にある文字列を変数に代入できるようになりました。
そして次にこの変数を第2引数の関数に渡して実行します。
そしてその結果も`IOMonad`を返すので、これを`return`して完了です。

この関数により2つのアクションが連結して、1つのコンテキストの中で入力から出力まで実行することができました。

Haskellでいうと以下の書き方と同様になると思います。
```haskell
main :: IO ()
main = do
    input <- getLine
    putStrLn input
```

- **`main`関数で実行する**

```go
func main() {
	action := bind(doInput(), doPrint)
	action()
}
```

実際にアクションを実行してみましょう。
第1引数には`IOMonad`型が欲しいので`doInput()`を渡します。
第2引数は`func(string) IOMonad`型が欲しいので、`doPrint`とし、実行したものではなく関数自体を引数に渡します。

これにより変数`action`には`doInput`と`doPrint`が一連の処理として実行された結果のアクションが代入されます。
そしてこのアクション自体が何かできるわけではないですが、この`action`が作成される過程で標準入力から入った値がコンテキスト内で標準出力まで渡るようになりました。

これによって`main()`内部で標準入力で取得した値を格納するような変数が存在せず、状態に依存したものをコード上から排除することができました！

最後にコード全体も載せておきます。

```go
package main

import (
	"bufio"
	"fmt"
	"os"
)

// IOMonadは、入出力操作をカプセル化するための関数型です。
type IOMonad func() string

// doInputは、標準入力から文字列を読み込むIOMonadを生成します。
func doInput() IOMonad {
	return func() string {
		sc := bufio.NewScanner(os.Stdin)
		sc.Scan()
		return sc.Text()
	}
}

// doPrintは、与えられた文字列を標準出力に出力するIOMonadを生成します。
func doPrint(s string) IOMonad {
	return func() string {
		fmt.Println(s)
		return ""
	}
}

// Bindは、IOMonadを実行し、その結果を別のIOMonadにバインド（適用）する関数です。
func bind(m IOMonad, f func(string) IOMonad) IOMonad {
	input := m()
	result := f(input)
	return result
}

func main() {
	// Bindで連結することで、時間依存のない標準入力と標準出力を実現できます。
	action := bind(doInput(), doPrint)
	action()
}
```

# 終わりに

モナドというものがどういうものか理解したかったので簡単でもいいから一から自分で実装してみよう、というのが動機で書き始めてみました。
これによって様々なモナド(`List`や`Maybe`など)に対する理解や、これらを操作する関数たちについても理解が深まりました。
何か間違いなどあれば教えていただけるとありがたいです。

まだ関数型プログラミングについてはカバーしきれていない部分も多いので引き続き勉強していこうと思います。
最後まで見てくださってありがとうございました！

# 参考
\[1\]:

https://ja.wikipedia.org/wiki/%E3%83%A2%E3%83%8A%E3%83%89_(%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0)

\[2\]:

https://techblog.ap-com.co.jp/entry/2022/12/28/175316

\[3\]:

https://www.infoq.com/jp/articles/Understanding-Monads-guide-for-perplexed/
