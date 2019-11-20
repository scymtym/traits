(cl:in-package #:traits)

(defclass named-mixin ()
  ((%name :initarg :name
          :reader  name)))

(defmethod print-items:print-items append ((object named-mixin))
  `((:name ,(name object) "~A")))

(defclass supertrait ()
  ((%trait     :initarg :trait
               :reader  trait)
   (%arguments :initarg :arguments
               :reader  arguments)))

(defclass trait-parameter (named-mixin
                           print-items:print-items-mixin)
  ((%trait       :initarg :trait
                 :reader  trait)
   (%lower-bound :initarg :lower-bound
                 :reader  lower-bound)
   (%upper-bound :initarg :upper-bound
                 :reader  upper-bound)))

(defclass trait-method (named-mixin
                        print-items:print-items-mixin)
  ((%lambda-list :initarg :lambda-list
                 :reader  lambda-list)))

(defclass trait (named-mixin
                 print-items:print-items-mixin)
  ((%direct-supertraits :initarg :direct-supertraits
                        :reader  direct-supertraits)
   (%parameters         :initarg :parameters
                        :reader  parameters)
   (%direct-methods     :initarg :direct-methods
                        :reader  direct-methods)))

(defmethod methods ((trait trait))
  (append (direct-methods trait)
          (mappend #'methods (direct-supertraits trait))))

;;;

(defmethod specializer-name ((thing trait-parameter))
  (let ((trait (trait thing)))
    (symbolicate (name trait) '#:- (name thing))))

(defmethod super-parameters ((thing trait-parameter))
  (flet ((one-super-parameter (supertrait)
           (when-let ((position (position thing (arguments supertrait))))
             (list (nth position (parameters (trait supertrait)))))))
    (mappend #'one-super-parameter (direct-supertraits (trait thing)))))

(defmethod emit ((thing trait-parameter))
  (let* ((trait       (trait thing))
         (class-name  (specializer-name thing))
         (super-names (map 'list #'specializer-name (super-parameters thing))))
    (eval (print `(defclass ,class-name ,super-names
                    ()
                    (:metaclass trait-class)
                    (:trait ,trait)
                    (:role ,thing))))))

(defmethod emit ((thing trait))
  (map nil #'emit (parameters thing)))

(defun check-implementation (class role trait)
  (let ((parameter (if (typep role 'symbol)
                       (find role (parameters trait) :test #'eq :key #'name)
                       role)))
    (every (lambda (trait-method)
             (format t "~A~%" trait-method)
             (let ((lambda-list (lambda-list trait-method))
                   (function    (symbol-function (name trait-method))))
               (labels ((method-applicable-p (method)
                          (format t "  ~A~%" method)
                          (loop :for specializer :in (c2mop:method-specializers method)
                                :for (nil p)     :in lambda-list
                                :always (or (not (eq (name parameter) (name p))) ; TODO hack
                                            (let ((result (subtypep class specializer)))
                                              (format t "    ~A [~A ~A] -> ~A~%" p class specializer result)
                                              result)))))
                 (let ((result (some #'method-applicable-p
                                     (c2mop:generic-function-methods function))))
                   (format t "  -> ~A~%" result)
                   result))))
           (methods trait))))

(defclass trait-generic-function (standard-generic-function
                                  function)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defun applicable-methods-due-to-traits (generic-function arguments)
  (labels ((specializer-compatible-p (specializer argument)
             (format t "~A | ~A~%" specializer argument)
             (if (typep specializer 'trait-class)
                 (check-implementation (class-of argument)
                                       (role specializer)
                                       (trait specializer))
                 (typep argument specializer)))
           (method-applicable-p (method)
             (every #'specializer-compatible-p
                    (c2mop:method-specializers method) arguments)))
    (remove-if-not #'method-applicable-p
                   (c2mop:generic-function-methods generic-function))))

(defmethod compute-applicable-methods
    ((generic-function trait-generic-function) (arguments t))
  (append (applicable-methods-due-to-traits generic-function arguments)
          (call-next-method)))

(defun applicable-methods-due-to-traits-classes (generic-function classes)
  (labels ((specializer-compatible-p (specializer argument-class)
             (format t "~A | ~A~%" specializer argument-class)
             (if (typep specializer 'trait-class)
                 (check-implementation argument-class
                                       (role specializer)
                                       (trait specializer))
                 (subtypep argument-class specializer)))
           (method-applicable-p (method)
             (every #'specializer-compatible-p
                    (c2mop:method-specializers method) classes)))
    (remove-if-not #'method-applicable-p
                   (c2mop:generic-function-methods generic-function))))

(defmethod c2mop:compute-applicable-methods-using-classes
    ((generic-function trait-generic-function) (classes t))
  (multiple-value-bind (methods definitivep) (call-next-method)
    (values (append (applicable-methods-due-to-traits-classes
                     generic-function classes)
                    methods)
            definitivep)))

#+sbcl (defmethod sb-pcl:specializer-type-specifier
           ((proto-generic-function trait-generic-function)
            (proto-method t)
            (specializer t))
         't)

(defclass trait-class (standard-class)
  ((%trait :reader  trait
           :writer  (setf %trait))
   (%role  :reader  role
           :writer  (setf %role))))

(defmethod shared-initialize :after ((instance   trait-class)
                                     (slot-names t)
                                     &key
                                     (trait nil trait-supplied-p)
                                     (role nil role-supplied-p))
  (when trait-supplied-p
    (setf (%trait instance) (first trait)))
  (when role-supplied-p
    (setf (%role instance) (first role))))

(defmethod c2mop:validate-superclass ((class trait-class)
                                      (superclass standard-class))
  t)

;;; Example 2: Ring
