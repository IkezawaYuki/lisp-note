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
    (append (edges-pair (caar islands) (caadr islands))
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
                                (if (intersection (edges-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
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
                              ((some (lambda (worm)
                                        (within-one n worm edge-alist))
                                      glow-worms)
                                '(lights!)))
                        (when (some #'cdr (cdr (assoc n edge-alist)))
                          '(sirens!))))))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))


(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))


(defun known-city-nodes()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                (let ((n (assoc node *congestion-city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))
                      n))
                (list node '?)))
          (remove-duplicates
            (append *visited-nodes*
                    (mapcan (lambda (node)
                              (mapcar #'car
                                      (cdr (assoc node
                                                  *congestion-city-edges*))))
                            *visited-nodes*)))))

(defun known-city-edges ()
  (mapcar (lambda (node)
    (cons node (mapcar (lambda (x)
                        (if (member (car x) *visited-nodes*)
                            x
                            (list (car x))))
                        (cdr (assoc node *congestion-city-edges*)))))
              *visited-nodes*))

(defun draw-known-city ()
  (upgraph->png "known-city" (known-city-nodes) (known-city-edges)))


(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))


(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                    (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
        (handle-new-place edge pos charging)
        (princ "That location does not exist!"))))


(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
          ((member 'wumps node) (if charging
                                      (princ "You found the Wumpus!")
                                      (princ "You ran into the Wumpus")))
          (charging (princ "You  wasted your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))

(defun hash-edges (edge-list)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x)
            (let ((node (car x)))
              (push (cdr x) (gethash node tab))))
          edge-list)
    tab))

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
                (unless (gethash node visited)
                  (setf (gethash node visited) t)
                  (mapc (lambda (edge)
                          (traverse edge))
                        (gethash node edge-tab)))))
                (traverse node))
              visited))

(princ (reverse
        (format nil "Add onion rings for only ~$ dollars more!" 1.5)))

(defun random-animal()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

(loop repeat 10
  do (format t "~5t~a ~15t~a ~25t~a~%"
        (random-animal)
        (random-animal)
        (random-animal)))

(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))

(output-stream-p *standard-output*)

(write-char #\x *standard-output*)

(input-stream-p *standard-input*)

(read-char *standard-input*)

(with-open-file (my-stream "data.txt" :direction :output)
  (print "my data" my-stream))

(with-open-file (my-stream "data.txt" :direction :input)
  (read my-stream))

(let ((animal-noises '((dog . woof)
                       (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
    (print animal-noises my-stream)))

(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (read my-stream))

(with-open-file (my-stream "data.txt" :direction :output :if-exists :error)
  (print "my data" my-stream))

(with-open-file (my-stream "data.txt" :direction :output
                                      :if-exists :supersede)
  (print "my data" my-stream))

(defparameter my-socket (socket-server 4321))

(defparameter my-stream (socket-accept my-socket))

(defparameter my-stream (socket-connect 4321 "127.0.0.1"))

(close my-stream)

(defparameter foo (make-string-output-stream))
(princ "This will go into foo. " foo)
(princ "This will go also go into foo." foo)

(define-condition foo()()
  (:report (lambda (condition stream)
            (princ "Stop FOOing around, numbskull!" stream))))

(defun bad-function ()
  (error 'foo))

(handler-case (bad-function)
  (foo () "somebody signaled foo!")
  (bar () "somebody signaled bar!"))


(unwind-protect (/ 1 0)
  (princ "I need to say 'flubyduby' matter what"))

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
                (coerce (list c1 c2) 'string)
                :radix 16
                :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  (labels ((f (lst)
              (when lst
                (case (car lst)
                  (#\% (cons (http-char (cadr lst) (caddr lst))
                              (f (cdddr lst))))
                  (#\+ (cons #\space (f (cdr lst))))
                  (otherwise (cons (car lst)(f (cdr lst))))))))
    (coerce (f (coerce s 'list))'string)))


(defun parse-params (s)
  (let ((i1 (position #\= s))
       ((i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s)))))

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params url (1+ x)))
        (cons url '()))))

(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
          (when i
            (cons (intern (string-upcase (subseq s 0 i)))
                  (subseq s (+ i 2)))))))
          (when h
            (cons h(get-header stream)))))


