(load "~/quicklisp/setup.lisp")

(ql:quickload "april")
(ql:quickload "str")

(defun /first (f pair)
  (cons (funcall f (car pair)) (cdr pair)))

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun april-run-and-print-solution (day data pgm1 pgm2)
  (format t "=== Day ~A ===~%Part 1:~%~A~%Part 2:~%~A~%"
          day
          (april:april-f (with (:state :output-printed :only :in ((a data)))) pgm1)
          (april:april-f (with (:state :output-printed :only :in ((a data)))) pgm2)))

(defun day1 ()
  (defun parse (input)
    (let ((parsed (reduce (lambda (acc x)
                            (if (equal x "")
                                (cons '() acc)
                                (cons (cons (parse-integer x) (car acc)) (cdr acc))))
                          (str:lines input)
                          :initial-value '(() . ()))))
      (coerce (mapcar (lambda (l) (coerce l 'vector)) parsed) 'vector)))
  (april-run-and-print-solution
   1
   (parse (file-get-contents "../inputs/day1.txt"))
   "⌈/+/¨a"
   "+/{(⊂3↑⍒⍵)⌷⍵}+/¨a"))
