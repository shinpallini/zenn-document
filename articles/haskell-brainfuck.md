---
title: "HaskellでBrainfuckを実装してみる(番外編)"
emoji: "🐈"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["Haskell", "Codespaces", "Brainfuck"]
published: true
---

# はじめに

最近投稿できなかったのですが、Haskellはちまちま触っていました。
その中でSNS上でBrainfuckというプログラミング言語のインタープリターを書いている方がいて、なんだこれは・・・と思い調べてみました。

https://ja.wikibooks.org/wiki/Brainfuck#:~:text=Brainfuck%E3%81%AF%E3%80%81%20Urban%20M%C3%BCller%E3%81%AB%E3%82%88%E3%81%A3%E3%81%A6,%E3%82%82%E3%81%97%E3%81%8F%E3%81%AF%E3%80%8C.bf%E3%80%8D%E3%81%A7%E3%81%99%E3%80%82

その方曰く「Brainfuckでプログラムを書くのは大変だが、Brainfuckのインタープリターを書くのは比較的簡単」とのことだったので、自分もHaskellの勉強もかねて作成してみました！

inputが1行で入力される必要がある・想定外の文字列が来た時に正しくエラーにならないなどの不足は色々ありますが、おおむね動くようになったので一旦公開しようと思います。

コードの全体は以下になります。
そして以後実装の詳細について簡単にコメントしていきます。

```haskell
import Data.Array
import Data.Char (chr)

type Index = Int

type Depth = Int

data Brain = Brain
  { memory :: [Int],
    pointer :: Int,
    arrayIndex :: Int,
    memoryIndex :: Int
  }
  deriving (Show)

-- convert from string to array
stringToArray :: String -> Array Int Char
stringToArray str = listArray (0, length str - 1) str

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []
replaceAtIndex n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceAtIndex (n - 1) newVal xs

modifyMemory :: Brain -> [Int]
modifyMemory brain
  | memIndex >= 0 && memIndex < length (memory brain) =
      replaceAtIndex memIndex (pointer brain) (memory brain)
  | otherwise = memory brain
  where
    memIndex = memoryIndex brain

run :: Array Int Char -> Brain -> IO ()
run array brain
  -- read all input
  | arrayIndex brain > length array - 1 = putChar '\0'
  -- memoryIndex is not negative
  | memoryIndex brain < 0 = putStrLn "Runtime error: Jumped beyond list bounds"
  | otherwise = do
      let token = array ! arrayIndex brain
      case token of
        '+' -> run array (brain {pointer = pointer brain + 1, arrayIndex = arrayIndex brain + 1})
        '-' -> run array (brain {pointer = pointer brain - 1, arrayIndex = arrayIndex brain + 1})
        '>' ->
          run
            array
            ( brain
                { memory = modifyMemory brain,
                  memoryIndex = nextMemoryIndex,
                  arrayIndex = arrayIndex brain + 1,
                  pointer = memory brain !! nextMemoryIndex
                }
            )
          where
            nextMemoryIndex = memoryIndex brain + 1
        '<' ->
          run
            array
            ( brain
                { memory = modifyMemory brain,
                  memoryIndex = prevMemoryIndex,
                  arrayIndex = arrayIndex brain + 1,
                  pointer = memory brain !! prevMemoryIndex
                }
            )
          where
            prevMemoryIndex = memoryIndex brain - 1
        '.' -> do
          putChar $ chr (pointer brain)
          run array (brain {arrayIndex = arrayIndex brain + 1})
        '[' -> case pointer brain of
          0 -> do
            let depth = 1
            jumpForward array brain depth
          _ -> run array (brain {arrayIndex = arrayIndex brain + 1})
        ']' -> case pointer brain of
          0 -> run array (brain {arrayIndex = arrayIndex brain + 1})
          _ -> do
            let depth = 1
            jumpBackward array brain depth

jumpForward :: Array Int Char -> Brain -> Depth -> IO ()
jumpForward array brain depth
  | arrayIndex brain >= length array = putStrLn "Runtime error: Jumped beyond array bounds"
  | depth == 0 = run array brain
  | otherwise =
      let nextIndex = arrayIndex brain + 1
          nextToken = array ! nextIndex
          newDepth = case nextToken of
            '[' -> depth + 1
            ']' -> depth - 1
            _ -> depth
          newBrain = brain {arrayIndex = nextIndex}
       in jumpForward array newBrain newDepth

jumpBackward :: Array Int Char -> Brain -> Depth -> IO ()
jumpBackward array brain depth
  | arrayIndex brain < 0 = putStrLn "Runtime error: Jumped beyond array bounds"
  | depth == 0 = run array brain
  | otherwise =
      let prevIndex = arrayIndex brain - 1
          prevToken = array ! prevIndex
          newDepth = case prevToken of
            ']' -> depth + 1
            '[' -> depth - 1
            _ -> depth
          newBrain = brain {arrayIndex = prevIndex}
       in jumpBackward array newBrain newDepth

main :: IO ()
main = do
  input <- getLine
  let brain =
        Brain
          { memory = replicate 100 0,
            pointer = 0,
            arrayIndex = 0,
            memoryIndex = 0
          }
  run (stringToArray input) brain
```

# 実装方針

まずは入力された文字列の読み取り状況などを保持する型をレコード構文で定義します。
入力された文字列を読み取ったり、その状態を記憶しておくものだと捉えて、言語の名前に合わせてこの型をBrainとしました。

そして入力は`getLine`で1行読み取り、これを`run`に渡すことで処理を開始します。
`run`の型は以下のように定義しました。

```haskell
run :: Array Int Char -> Brain -> IO ()
```

引数は

- 入力文字列を`Char`型のArrayに変換したもの
- Brain型の変数

に取っています。

`run`のが返す値の型は` [Char] `という選択肢もあるのですが、今回文字列を標準出力する文字 `.` を受け取った時点で出力する設計にしたかったので、型を`IO ()`としています。

次に文字列をすべて読み取るために再帰関数として書いており、文字を1つ読んだら`arrayIndex`に1加算することで読み進めていきます。
あとは入力された文字に合わせて`pointer`の値を加算したり、次のメモリを読み取りに行ったりしています。

`.`が入力されたときは`do`構文を使って`putChar`した後再び`run`を呼んでいます。

残りはループが定義されている`[`、`]`ですが、それぞれ専用の`jumpForward`、`jumpBackward`を定義しています。
メモリが特定の値(0かどうか)でループの深さが一致したものにのみジャンプするようにしています。

実行結果は以下のようになりました。

```powershell
runghc .\brainfuck\v2\main.hs
+++++++++[>++++++++>+++++++++++>+++>+<<<<-]>.>++.+++++++..+++.>+++++.<<+++++++++++++++.>.+++.------.--------.>+.>+.
Hello World!
```

# 終わりに

競技プログラミングでHaskellを練習しているおかげか、何かしらものを作ってみようと思ったときにHaskellで書けるようになったのはすごい嬉しいです！
引き続き競技プログラミングでもそうじゃなくても、Haskellに触れてプログラムを書いていきたいです。
