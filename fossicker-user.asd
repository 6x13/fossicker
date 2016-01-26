(defsystem #:fossicker-user
  :defsystem-depends-on ("fossicker")
  :class fossicker-configuration:system

  ;; CONFIGURATION
  
  :legend
  (("_b_" "button")
   ("_n_" "normal")
   ("_p_" "pressed")
   ("_e_" "enabled"))
  
  :projects
  ((:file "~/dev/lisp/local-projects/fossicker/test/6x13.proj"
    :root "~/dev/lisp/local-projects/fossicker/"
    :class 'project)
   (:file "~/dev/lisp/local-projects/fossicker/test/test.proj"))

  :depends-on
  ("fossicker/plugins/all"))
