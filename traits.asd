(defsystem "traits"
  :depends-on ("closer-mop"
               "alexandria"
               "utilities.print-items")
  :components ((:module     "src"
                :serial     t
                :components ((:file       "package")
                             (:file       "traits")
                             (:file       "macros")))))
