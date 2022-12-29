#### Lispではグローバルに定義される変数をトップレベル変数と呼ぶ。

#### 新しいトップレベル定義にはdefparameterを使う
```
(defparameter *small* 1)
```
 \*small*についている\*は単なる習慣で、意味はない。
 だが、ローカル変数と区別するのに便利なので、使うのは推奨。

#### defparameterを使ってグローバル変数を定義すると、その変数が既に定義されていた場合、元の値は上書きされる。

```
> (defparameter *foo* 5)
*FOO*
5
> (defparameter *foo* 6)
*FOO*
6
```
#### defvar を使うと上書きされない

```
> (defvar *foo* 5)
*FOO*
5
> (defvar *foo* 6)
*FOO*
5

```

#### コマンドとその引数をいちいち括弧に入れてやらないとならない

#### 空白と改行は全て無視される

#### 関数はdefunを使って定義できる
```
(defun function_name (arguments)
 ...)

 > (defun guess-my-number ()
(ash (+ *small* *big*) -1))
GUESS-MY-NUMBER
```

#### 関数の後の空の括弧()は、この関数がパラメータを取らないことを示している。

#### ash関数は、渡された数値を2進数で考え、ビットを右または左にシフトする。

#### Lispでは整数に上限はない。
```
[4]> (ash 11 1)
22
[5]> (ash 11 -1)
5
```

#### 関数の最後の指揮の値が自動的に返されるので、return は書く必要はない

#### setf関数を使ってグローバル変数の値を書き換える

#### ローカル変数を定義するにはletコマンドを使う
```
(let (変数定義)
...本体...)
```

#### ローカル変数はfletコマンドで定義できる
```
(flet ((関数名(引数)
        ...関数本体...))
      ...本体...)
```

#### 一つのfletコマンドで複数のローカル関数を一緒に宣言するには、単にコマンドの最初の部分に複数の関数宣言を書けば良い。

#### ローカル関数の中で同じスコープで定義されるローカル関数名を使いたい場合は、labelsコマンドを使う。

```
(labels ((a (n)
            (+ n  5))
          (b (n)
             (+ (a n) 6)))
            (b 10))
```

#### シンボルはLispの最も基本的なデータ型。

#### eqで二つのシンボルが等しいかどうかを調べることができる

#### 文字列を表示するにはprincという関数を使う

#### Common Lispはコードを読み込む際に二つのモードを持っている

#### コードモードは読んでいるのはコードだと解釈する

#### データモードでは読んでいるコードは全てデータとして扱われる

```
> '(expt 2 3)
(expt 2 3)
```

#### リストはLispの中でもとりわけ重要。Lispのコード、データを繋ぎ合わせる。
```
(expt 2 3)
このコード片にはシンボル(expt)と二つの数値が使われていて、それが括弧で括られたリストとしてまとめられている
```

#### Lispのリストはコンスセルでつなぎ合わされている

#### コンスセルを扱う最も基本となる関数が三つある。cons、car, cdr。

#### cons関数　二つのデータをLispプログラムの中で結びつけたい時に使うのがcons関数。

#### consは一つのコンスセルを返す。コンスセルの表現は、つなげられた要素の間にドットを置いて、全体を括弧で囲んだもの。通常のリストとは混同しないこと。

#### 他のデータの代わりにnilを渡すと、nilは出力されない

#### 空のリストはCommon Lispではシンボルnilと同じ

#### Lispプログラムは、consを使うことを「（何かを）コンスする」ということがある。
```
(cons 'pork '(beef chicken))
(PORK BEEF CHICKEN)
```

#### Lispではコンスセルの連なりとリストとは全く同じもの

#### 関数はcarは、セルの最初のスロットにあるデータを取り出すのに使う
```
> (car '(pork beef chicken))
PORK
```

#### 関数cdrは２番目のスロットの値を取り出す。リストの場合は、リストの残りの部分を返すと言ってもいい。
```
> (cdr '(pork beef chicken))
(BEEF CHICKEN)
```


#### cadrは、２番目のコンスセルの最初のスロットの値、すなわちリストの２番目の要素を返す。

#### Common Lispには、この基本３原則cons, car, cdrを使って組み立てられたたくさんの関数が存在する。その一つはlist関数

#### List関数は長いリストを一度に作ることができる

#### cadr ２番目以降の１番目

#### 条件式の評価において空のリストを偽として扱うこと
```
> (if '()
      'i-am-true
      'i-am-false)
> (if '(1)
      'i-am-true
      'i-am-false)
```

```
リストを食べる関数の代表
> (defun my-length (list)
   (if list
       (1+ (my-length (cdr list)))
        0))

> (my-length '(list with four symbols))
```

```
1: (eq '() nil) ==> T
2: (eq '() ()) ==> T
3: (eq '() 'nil) ==> T

```

#### ifの持つ二つの式のうち、どちらか一方だけが実際に評価される。

#### prognという特別なコマンドを使って、一つの式の中に余分なコマンドを押し込むことができる。
```
> (defvar *number-was-odd* nil)

> (if (oddp 5)
      (progn (setf *number-was-odd* t)
              'odd-number)
      'even-number)
```

#### 暗黙のprogn。whenとunless
```
> (defvar *number-is-odd* nil)
> (when (oddp 5)
    (setf *number-is-odd* t)
    'odd-number)

> (unless (oddp 4)
    (setf *number-is-odd* nil)
    'even-number)
```

#### when は条件が真の時に囲まれた式を全て実行する。

#### unlessは条件が偽の時に囲まれた式を全て実行する。

#### 万能条件コマンドcond
#### condの本体はいくつも重なった括弧を使って、それぞれの条件を分けている。括弧で分けられた各分岐について、その最初の式がその分岐を選ぶかどうかを決める条件になる。

#### caseによる分岐
```
(defun pudding-eater (person)
  (case person
    ((henry) ()
             ())
    ((johnny) ()
              ())
    (otherwise ())))
```

#### caseコマンドはeqを比較に使うので、シンボルの値で分岐するのに使われる。文字列の値で分岐することはできないことに注意。

#### 条件分岐コマンドandとorは、単純な論理オペレーター
```
> (and (oddp 5)(oddp 7)(oddp 9))
T

> (or (oddp 4)(oddp 7)(oddp 8))
T

```

#### orに与えられた式のうち真なるものが見つかったら直ちに、残りの式を評価せずにすぐに真を返す

#### andに与えられた式のうち偽になるものが見つかったら直ちに、残りの式を評価せずにすぐに偽を返す

#### Lispコマンドmemberはリスト中に、ある要素が含まれているかどうを調べる関数
```
> (member 1 '(3 3 4 1 4 9))
(1 4 9)
```

#### find-if関数は、最初の引数に別の関数を受け取る
```
> (find-if #'oddp '(2 4 5 6))

```
#### find-if関数を条件分岐にはつかえない。nilを探すと、nilが帰り、条件分岐では偽となってしまう。

#### 比較についてのコンラッドのルール
```
1. シンボル同士は常にEQで比較すべし。
2. シンボル同士の比較でなければEQUALを使え。
```
#### eqはシンボルの比較しか使えない

#### equalは二つのものが「同じ見かけをしている」かどうかを教えてくれる

#### eqlコマンドeqと似ているが、eqと違って数値と文字も比較できる

#### equalpはequalとほぼ同じだが、文字列については大文字小文字の使い方が異なるものでも比較できる、整数と浮動小数店を比較できる。
```
> (equalp "Bob Smith" "bob smith")
```
#### = (イコール記号)は数値を扱う

#### string-equalは文字列を扱う

#### char-equalは文字を扱う

```
(defparameter *nodes* '((living-room) (you are in the living-room)))

```

#### assoc関数はリスト中からキーを元に欲しい要素を抜き出す

#### mapcarは引数に他の関数とリストを受け取って、リストの要素の一つ一つについてそれを引数として受けとった関数を呼び出す

```
> (mapcar #'sqrt '(1 2 3 4 5))
(1 1.4142135 1.7320508 2 2.236068)
```

#### mapcarのように他の関数を引数として受け取る関数は、高階関数と呼ばれる

```
> (mapcar #'car '((foo bar) (baz qux)))
(FOO BAZ)
```
#### \#'はfunctionオペレーターの略記
```
> (mapcar (function car) '((foo bar) (baz qux)))
(FOO BAZ)
```
#### Common Lispでは、関数を値として扱う場合にfunctionオペレータを使ってそのことを明示する
```
> (let ((car "Honda Civic"))
    (mapcar #'car '((foo bar) (baz qux))))
```

```
> (cdr (assoc 'living-room *edges*))
((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
```

```
> (mapcar #'describe-path '((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)))
((THERE IS A DOOR GOING WEST FROM HERE.) (THERE IS A LADDER GOING UPSTAIRS FROM HERE.))
```


#### Common Lispは変数名と函数名を別々に管理している。変数の名前空間と関数の名前空間があると言ってもいい。

#### applyに関数とリストを渡すと、あたかもそのリストの各要素を引数として関数を呼び出したかのように動作する。

```
applyはネストしたリスト'((mary had)(a)(little lamb))とappendをガムテープでくっつけているようなもの
> (apply #'append '((mary had)(a)(little lamb)))
(MARY HAD A LITTLE LAMB)
```
#### 関数がnilか真の値を返す場合、CommonLispではその関数の名前の最後にpをつけるという習わしがある

#### データの一部に計算された情報を埋め込んで返す機能を準クオートと呼ぶ。バッククオートないでコンマ文字を使うと、一部分だけをコードモードに戻せる => アンクオートするという。


#### find関数はリストから与えられた要素を探す関数
#### Common Lispではfindなど多くの関数が、関数呼び出しの後ろに特別な引数を渡すことで使えるようになる。

```
シンボルyをcadrに持つ最初の要素をリストから探し出す
> (find 'y '((5 x)(3 y)(7 z)) :key #'cadr)
(3 Y)
```

#### pushコマンドはリストを保持している変数に新しい要素を付け加える
```
> (defparameter *foo* '(1 2 3))
*FOO*
> (push 7 *foo*)
(7 1 2 3)

pushコマンドはsetfコマンドを使って作られた簡易関数。
pushを(setf *foo* (cons 7 *foo*))
```

#### alistの中の値を置き換えたければ、新しい要素をリストにpushするだけでいい。assocはもっとも新しい値だけを返す。

#### pushとassocを使って、以前の値を残したまま、alistの値が変更されたかのように見せられる。


#### print関数は、単に渡したものをコンソールに表示してくれる
```
> (print "foo")

"foo"
"foo"

print関数が、"foo"を２回出力したわけではなく、最初の"foo"がprintが実際に表示したもので、２番目の"foo"はREPLが返したもの
```

#### print関数はそれぞれの引数を別々の行に表示したがprin1は改行しない
```
> (progn (prin1 "this")
         (prin1 "is")
         (prin1 "a")
         (prin1 "test"))
"this""is""a""test"
```
#### read関数はLispの動作を一旦止めて、ユーザーがREPLに何かタイプするのを待つ。
```
> (defun say-hello ()
    (print "Please type your name:")
    (let ((name (read)))
      (print "Nice to meet you, ")
      (print name)))
```

```
> (defun add-five ()
    (print "please enter a number:")
    (let ((num (read)))
      (print "When I add five I get")
      (print (+ num 5))))
```

#### Lispで文字を表したい場合、\#\\を置く

#### CommonLispのシンボルは大文字小文字を区別しないが、区別したい場合は|で囲めば良い。

#### princ関数はLispのあらゆるデータ型をとり、できるだけ人が読みやすい形でそれを表示する。

#### printの良いところは出力したものを後でいつでも「読み戻す」ことが可能な形で表示すること。

#### read-lineは文字と文字意外のことは何一つ知らず、enterキーが押されるまではどんな文字でも粛々と保存する。
```
> (defun say-hello ()
    (princ "Please type your name:")
    (let ((name (read-line)))
      (princ "Nice to meet you, ")
      (princ name)))
```
```
> '(+ 1 2)
(+ 1 2)
> (+ 1 2)
3
```

```
> (defparameter *foo* '(+ 1 2))
*FOO*
> (eval *foo*)
```

#### CLISPでは無限ループから抜けるにはCtrl-Cをタイプし、続けて出てくるプロンプトに:aと答えること。

#### concatenateコマンドは文字列を結合する

#### シングルクオートはLispのコマンドquoteの単なる略記。'fooと書くのは(quote foo)と書くのと同じ

#### prin1-to-string関数とその補助関数は、シンボルを文字列に変換する。結果をスクリーンにではなく、文字列として返す

#### coerce関数は文字列を文字のリストへと変換する

#### prin1の1は一行にとどまることを示す。

#### Lispでは関数は、表示したり引数として渡したりすることができる。（関数は第一級の値である）

```
> (lambda (n) (/ n 2))

> (mapcar (lambda (n) (/ n 2)) '(2 4 6))
```

#### lambdaの引数は評価されずにlambdaに渡される。つまりlambdaは本物の関数ではない。これはlambdaと呼ばれる。

#### lambdaが返す値は通常のLisp関数である。

#### 関数を値として渡すことを多用するプログラムスタイルを、高階プログラミングと呼ばれる。

#### リストはコンスセルの連なり。Lispがリストをどう表示しようが、根本的には、それがコンスセルの連なりであることは変わらない。

```
> (cons 1 (cons 2 3))
(1 2 . 3)
正式なリストの最後にあるべきnilが見つからなかったことを示すため、Lispは最後の要素の前にドットを置いて表示した。このドットによって、Lispは「君がくれた構造をリストとして表示しようとしたんだけれど、リストの一番最後にnilではなくて3を見つけたんだ」と言っている
```

#### nil以外の値でリストが終端しているものはドットリストと呼ばれる。

#### ドットリストがよく使われるのは対を簡潔に表現するため。

#### 最後をnilではなく、Listの先頭をさすものを循環リストという。

#### CommonLisp環境で循環リストを試す前には、必ず(setf *print-cirlce* t)を実行する。*print-circle*が真になっていると、各コンスセルが既に表示されたものかどうかを逐一チェックすることで、無限ループに入るのを避ける。

#### 連想リスト、別名alist。
```
(defparameter *drink-order* '((bill . double-espresso)
                              (lisa . small-drip-coffee)
                              (john . medium-latte)))
```

```
> (push 'lisa . large-mocha-with-whipped-cream) *drink-order*)
push関数はListの先頭に要素を追加する。
連想リストでは最初に見つかったキーの値が優先される。
```

```
> (defparameter *house* '((walls (morter (cement)
                                        (water)
                                        (sand))
                                (briks))
                          (windows (glass)
                                  (flame)
                                  (curtains))
                          (roof (shingles)
                                (chimney))))
```