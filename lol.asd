(defpackage :lol-system (:use :cl :asdf))
(in-package :lol-system)

(defsystem lol
  :name "let-over-lambda"
  :version "1.0"
  :author "Jonathon McKitrick"
  :description "Let Over Lambda Proof of Concepts"
  :serial t
  :components
  ((:file "defpackage")
   (:file "lol-utils")
   (:file "lol-macros")
   (:file "main")))
