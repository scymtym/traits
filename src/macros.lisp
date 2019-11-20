(cl:in-package #:traits)

(defvar *traits* (make-hash-table :test #'eq))

(defun find-trait (name)
  (gethash name *traits*))

(defun (setf find-trait) (new-value name)
  (setf (gethash name *traits*) new-value))

(defun ensure-trait (name class make-initargs)
  (let ((trait (find-trait name)))
    (cond ((not trait)
           (setf trait             (make-instance class :name name)
                 (find-trait name) trait))
          ((not (eq (class-of trait) class))
           (change-class trait class)))
    (let ((initargs (funcall make-initargs trait)))
      (apply #'reinitialize-instance trait initargs))))

(defmacro deftrait ((&rest parameters) name (&rest supertraits)
                    &body declarations)
  (let ((parameter-names  '())
        (parameter-forms  '())
        (supertrait-forms '())
        (method-forms     '()))
    (loop :for parameter  :in parameters
          :for variable-name = (gensym "PARAMETER") ; class-name =   (symbolicate name '#:- parameter)
          :for form       =   `(make-instance 'trait-parameter :trait trait
                                                               :name  ',parameter)
          :do (appendf parameter-names (list variable-name))
              (appendf parameter-forms (list form)))
    (flet ((parse-supertrait (supertrait)
             (destructuring-bind (name &rest arguments)
                 (ensure-list supertrait)
               (let ((arguments (loop :for argument :in arguments
                                      :collect (if-let ((position (position argument parameters)))
                                                 (nth position parameter-names)
                                                 `',argument))))
                 (push `(make-instance 'supertrait :trait     (find-trait ',name)
                                                   :arguments (list ,@arguments))
                       supertrait-forms)))))
      (map nil #'parse-supertrait supertraits))
    (flet ((parse-lambda-list (lambda-list)
             (loop :for (name parameter) :in lambda-list
                   :collect `(list ',name ,(nth (position parameter parameters) parameter-names))))) ; HACK
      (loop :for (operator name lambda-list) :in declarations
            :unless (eq operator :method)
            :do (error "~S is not a known trait declaration operator" operator)
            :do (push `(make-instance 'trait-method
                                      :name ',name
                                      :lambda-list (list ,@(parse-lambda-list lambda-list)))
                      method-forms)))
    `(let ((trait (ensure-trait
                   ',name 'trait
                   (lambda (trait)
                     (let ,(map 'list #'list parameter-names parameter-forms)
                       (list :direct-supertraits (list ,@supertrait-forms)
                             :parameters         (list ,@parameter-names)
                             :direct-methods     (list ,@method-forms)))))))
       (emit trait)
       trait)))
