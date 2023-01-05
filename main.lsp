#|
  プレイヤーが行くことができる場所
 |#
(defparameter *nodes*  '((living-room (you are in the living-room.
                            a wizard is snoring loundly on the couch.))
                         (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                         (attic (you are in the attic.
                            there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))
  
#|
  場所を行き来する経路
|#
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a, (caddr edge) going, (cadr edge) from here.))


(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


(defparameter *objects* '(whiskey bucket frog chain))


(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
              (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
              `(you see a ,obj on the floor.)))
    (apply #'append(mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location*  *objects* *object-locations*)))


(defun walk (direction)
  (let ((next (find direction
                      (cdr(assoc *location* *edges*))
                      :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
          '(you cannot go that way.))))


(defun pickup (object)
  (cond ((member object
                  (objects-at *location* *objects* *object-locations*))
        (push (list object 'body) *object-locations*)
          `(you are now carrying the ,object))
        (t '(you cannot get that.))))


(defun inventry()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun game-repl()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read()
  (let ((cmd (read-from-string
                (concatenate 'string "(" (read-line) ")"))))
      (flet ((quote-it (x)
              (list 'quote x)))
        (cons (car cmd)(mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))


(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                        (prin1-to-string lst))
                                'list)
                              t
                              nil)
                          'string))
                      (fresh-line))

(defparameter *wizard-nodes*  '((living-room (you are in the living-room.
                            a wizard is snoring loundly on the couch.))
                         (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                         (attic (you are in the attic.
                            there is a giant welding torch in the corner.))))


(defparameter *wizard-edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
        ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
              edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))


(with-open-file (my-stream
                 "testfile.txt"
                 :direction :output
                 :if-exists :supersede)
  (princ "Hello File!" my-stream))


(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
              (mapc (lambda (edges)
                      (unless (assoc (car edges) (cdr lst))
                        (fresh-line)
                        (princ (dot-name (caar lst)))
                        (princ "--")
                        (princ (dot-name (car edge)))
                        (princ "[label=\"")
                        (princ (dot-label (cdr edge)))
                        (princ "\"];")))
                      (cdar lst)))
              edges))


(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))


(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))


(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edges-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edges-pair (random-node) (random-node)))))


(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                    (eql (car x) node))
                  edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
                (unless (member node visited)
                  (push node visited)
                  (mapc (lambda (edge)
                          (traverse (cdr edge)))
                        (direct-edges node edge-list)))))
    (traverse node))
  visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
                (let* ((connected (get-connected (car nodes) edge-list))
                       (unconnected (set-difference nodes connected)))
                       (push connected islands)
                       (when unconnected
                          (find-island unconnected)))))
          (find-island nodes))
        islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
                      collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
  (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                              :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (with-one x b edge-alist))
            (neighbors a edge-alist))))


(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                          collect (random-node))))
    (loop for n from 1 to *node-num*
        collect (append (list n)
                        (cond ((eql n wumpus) '(wumpus))
                              ((within-two n wumpus edge-alist) '(blood!)))
                        (cond ((member n glow-worms)
                                '(glow-worms))
                              ((some ())))))))

