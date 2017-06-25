(in-package #:lol)

(defun make-dlambda ()
  (dlambda
   (:foo (x) (format t "X: ~A~%" x))))

(defun foo ()
  (format t "Foo.~%"))

(setf (symbol-function 'd-foo) (make-dlambda))

(pandoriclet (a b c)
			 (format t "PLET~%")
			 )






(defun nlet-tail-fact (n)
  (nlet-tail fact ((n n) (acc 1))
			 (if (zerop n)
				 acc
			   (fact (- n 1) (* acc n)))))

(defun nlet-tail-fact! (n)
  (nlet-tail! fact ((n n) (acc 1))
				  (if (zerop n)
					  acc
					  (fact (- n 1) (* acc n)))))

(defmacro foo2 ((arg1) &body body)
  `(progn
	 (format t "Hello, world: ~A~%" ,arg1)
	 ,@body))

(defun foo3 (arg)
  (format t "Foo3 got an arg: ~A~%" arg))

(defvar temp-special 88)
(defvar baz 55)

(defun test-ml ()
  (foo2 (9)
	(format t "Did it work?~%"))
  (foo3 42)
  (macrolet ((foo2 ((arg1) &body body)
			   `(progn
				  (format t "Hello, Matrix: ~A~%" ,arg1)
				  ,@body))
			 (foo3 (arg1)
			   `(format t "Hello, Foo3 Matrix: ~A~%" ,arg1)
			   ))
	(foo2 (8)
	  (format t "Did it work?~%"))
	(foo3 99))
  (let ((baz 66))
	(symbol-macrolet ((bar temp-special))
	  (foo3 bar)
	  (let ((temp-special 77))
		(foo3 bar)))
	(foo3 baz))
  (foo3 baz))
