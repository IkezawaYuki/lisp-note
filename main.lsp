(defparameter *nodes*  '((living-room (you are in the living-room.
                            a wizard is snoring loundly on the couch.))
                         (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                         (attic (you are in the attic.
                            there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))
  
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))
#|
    データの一部に計算された情報を埋め込んで返す機能を準クオートと呼ぶ
    バッククオートないでコンマ文字を使うと、一部分だけをコードモードに戻せる
    => アンクオートするという。
|#
(defun describe-path (edge)
  `(there is a, (caddr edge) going, (cadr edge) from here.))


(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

```
> (cdr (assoc 'living-room *edges*))
((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
```

```
> (mapcar #'describe-path '((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER)))
((THERE IS A DOOR GOING WEST FROM HERE.) (THERE IS A LADDER GOING UPSTAIRS FROM HERE.))
```


