(defsystem #:fossickerrc
  :defsystem-depends-on (#:fossicker)
  :class "FOSSICKER:CONFIGURATION"

  ;; CONFIGURATION
  
  :legend
  (("_b_" "button")
   ("_n_" "normal")
   ("_p_" "pressed")
   ("_e_" "enabled"))
  
  :projects
  ((:file "~/dev/lisp/local-projects/fossicker/test/6x13.blog"
    :root "~/dev/lisp/local-projects/fossicker/")
   (:file "~/dev/lisp/local-projects/fossicker/test/test.project"))

  :depends-on
  ("fossicker/plugins/inconvenience"
   "fossicker/plugins/all"))
