#lang racket

(require srfi/1)
(require racket/match)
(require racket/string)

(define (part1)
  (let [[is (indexes-of (map (lambda (ps) (smaller (first ps) (second ps)))
                             (parse-pairs))
                        'left)]]
    (+ (length is) (apply + is))))

(define (part2)
  (let* [[div1 '((2))]
         [div2 '((6))]
         [sorted (sort (cons* div1 div2 (append* (parse-pairs)))
                       (lambda (p1 p2) (eq? 'left (smaller p1 p2))))]]
    (* (+ 1 (index-of sorted div1))
       (+ 1 (index-of sorted div2)))))

(define (smaller px py)
  (match* (px py)
    [((cons x xs) (cons y ys)) (match (smaller x y)
                                 ['neither (smaller xs ys)]
                                 [r        r])]
    [('() '())                 'neither]
    [('() _)                   'left]
    [(_ '())                   'right]
    [((cons x xs) y)           (smaller (cons x xs) (list y))]
    [(x (cons y ys))           (smaller (list x) (cons y ys))]
    [(x y)                     (cond [(< x y) 'left]
                                     [(< y x) 'right]
                                     [else    'neither])]))

(define (parse-pairs)
  (define (go acc lines)
    (match lines
      [(list* l1 l2 _ lines) (go (cons (list (parse-packet l1) (parse-packet l2)) acc) lines)]
      [(list l1 l2)          (go acc (list l1 l2 ""))]
      ['()                   (reverse acc)]))
  (let* [[f (open-input-file "../inputs/day13.txt")]
         [lines (cdr (unfold (lambda (line) (eof-object? line))
                             identity
                             (lambda (_) (read-line f))
                             null))]]
    (go '() lines)))

(define (parse-packet s)
  (define (go xs s)
    (match s
      ["" (first xs)]
      [s #:when (string-prefix? s ",") (go xs (substring s 1))]
      [s #:when (string-prefix? s "[") (match (go '() (substring s 1))
                                         [(cons ys s2) (go (cons ys xs) s2)])]
      [s #:when (string-prefix? s "]") (cons (reverse xs) (substring s 1))]
      [(regexp #rx"^[0-9]+" (list s1))
       (go (cons (string->number s1) xs) (substring s (string-length s1)))]))
  (go '() s))
