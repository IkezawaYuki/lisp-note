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

#### substitute-ifは与えられたテスト関数の結果によって値を置き換える関数

#### digit-char-p は文字列中の文字が数字かどうかを判定する

#### 値をとってその値が何かの条件を満たすかどうかの真偽を判定する関数はしばしば述語と呼ばれる

#### substitute-ifはリストも処理できる
```
> (substitute-if 0 #'oddp '(1 2 3 4 5 6 7 8 9))
```

#### complementは渡されたものを反対にする関数

#### Common Lispにはsubstitute-if-notという関数もあるが、notで終わっている関数は非推奨

#### write-to-string関数において、:pretty引数はキーワード引数の一例

#### mapcはmapcarの変種。mapcarとちがって結果のリストを返さない

#### Lispでは引数を取らない関数がよく使われ、これらの関数は正式には零項(nually)関数と呼ばれる

```
> (with-open-file (my-stream
                 "testfile.txt"
                 :direction :output
                 :if-exists :supersede)
  (princ "Hello File!" my-stream))

:direction引数には:outputが渡され、ファイルを読み込みではなく書き出し用にオープンすることを示す
:if-exists引数には:supersedeが渡され、同名のファイルが既に存在していた場合は以前の内容を捨てるように指示する。
```

#### Common Lispのシンボルにはコロンで始まるものがある。キーワード引数の名前もそうでいつでもコロンで始まる。
```
> (let ((cigar 5))
    cigar)
```

#### maplist関数はmapcar関数と似ているけれど、リスト中の現在の要素だけでなく、リストの残り全てを見ることができる関数

#### 
```
> (loop repeat 10
        collect 1)
loopコマンドの中で、repeatを使って何回ループするのかを指定し、collectで１回ごとに集めるオブジェクトを指定する。

> (loop for n from 1 to 10
        collect (+ 100 n))
```

```
> (let ((a 5)
        (b (+ a 2)))
    b)

> (let* ((a 5)
         (b (+ a 2)))
    b)
```

#### mapcan関数はmapcarの仲間。mapcarと違い、mapcanに渡す関数は必ずリストを返さないとならない。mapcanは返されたリストを全てつなぎ合わせたリストを返す。

#### 配列を作るには、配列の大きさを指定して、make-arrayコマンドを呼ぶ
```
> (make-array 3)
#(NIL NIL NIL)

先頭の#で配列はリストと区別される
```

#### 配列から要素をより出したり入れたりするにはaref関数を使う。
```
> (defparameter x (make-array 3))
X
> (aref x 1)
NIL

１番目の要素（ゼロ番から数えて）を取り出す
```

#### 値を配列にセットするには、arefをsetfコマンドと一緒に使う。
```
> (defparameter x (make-array 3))
X
> (setf (aref x 1) 'foo)
FOO
> x
#(NIL FOO NIL)
> (aref x 1)
FOO
```

#### setfとarefの組み合わせは、ジェネリックプログラミングと呼ばれるCommonLispの機能の一例。

#### リストでできることはほとんど全て配列でも実装できる。ただ、特定の要素にアクセスするのに、配列はリストよりもずっと速い

#### nthは通常のリストに対して、指定の場所をアクセスする関数。
```
> (nth 1 '(foo bar baz))
BAR
```
#### 新しいハッシュテーブルは要素をキーと値の組み合わせで格納する。

#### キーを使って要素を取り出すのはgethash関数
```
> (defparameter x (make-hash-table))
X
> x
#S(HASH-TABLE :TEST FASTHASH-EQL)
> (gethash 'yup x)
NIL ;
NIL

> (setf (gethash 'yup x) '25)
25
> (gethash 'yup x)
25 ;
T

```

#### round関数を呼ぶと引数を丸めた数が得られるが、その時に丸めの余りも同時に計算される。

#### 自分で作った関数から複数の値を返すにはvaluesを使う。

#### multiple-value-bindを使うと、通常なら捨てられてしまう余分な値をつかまえることができる。

#### 構造体を定義するにはdefstructコマンドを使う
```
> (defstruct person
             name
             age
             waist-size
             favorite-color)

```

```
> (defparameter *bob* (make-person :name "Bob"
                                   :age 35
                                   :waist-size 32
                                   :favorite-color "blue"))
> *bob*
#S(PERSON :NAME "Bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue")
> (person-age *bob*)
35
> (setf (person-age *bob*) 36)
```

#### シーケンス関数とは、データ型、リスト、配列、文字列を統一的に扱うことができる。
```
length 長さを調べる
find-if 与えた述語を満たす最初の要素を見つける
count 特定の要素がいくつシーケンス中にあるかを数える
position 特定の要素がシーケンスのどの位置にあるか教える
some シーケンス中に条件を満たす要素が最低ひとつはあるか調べる
every シーケンス中の要素全てが条件を満たすか調べる
```

#### reduceでリスト中の要素すべてを足し合わせる
```
> (reduce (lambda (best item)
            (if (and (evenp item) (> item best))
                item
                best))
            '(7 4 6 5 2)
            :initial-value 0)

> (defun sum (lst)
    (reduce #'+lst))
```

#### map関数はmapcarと同じで各要素を引数に渡した関数を読んで結果を集めるというもの。mapcarがリストにしか使えないのに対して、map関数は全てのシーケンスに使える。map関数はもうひとつ、返り血としてどのシーケンスの値を返すかという引数を取る。
```
> (map 'list
      (lambda (x)
        (if (eq x #\s)
            #\S
            x))
            "this is a string")

(#\t #\h #\i #\S #\Space #\i #\S #\Space #\a #\Space #\S #\t #\r #\i #\n #\g)
```

