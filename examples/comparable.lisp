(cl:defpackage #:traits.examples.comparable
  (:use
   #:cl
   #:traits))

(cl:in-package #:traits.examples.comparable)

(defvar comparable)
(deftrait (x) comparable ()

  (:method compare ((left x) (right x))))

(traits::emit comparable)

(defgeneric compare (left right))

(defmethod compare ((left integer) (right integer))
  (cond ((= left right)
         '=)
        ((< left right)
         '<)
        (t
         '>)))

(defmethod compare ((left string) (right string))
  (cond ((string= left right)
         '=)
        ((string< left right)
         '<)
        (t
         '>)))

(progn
  (fresh-line)
  (traits::check-implementation (find-class 'integer) 'x comparable)
  (fresh-line)
  (traits::check-implementation (find-class 'string) 'x comparable))

(defgeneric maybe-swap (left right)
  (:generic-function-class trait-generic-function))

(defmethod maybe-swap ((left comparable-x) (right comparable-x))
  (compare left right))

(defmethod maybe-swap ((left t) (right t))
  (compare left right))

(maybe-swap 1 2)

(maybe-swap "bar" "foo")
