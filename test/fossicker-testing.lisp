(ql:quickload :fossicker)
(fossicker::load-config)
(fossicker::add-project "~/dev/lisp/local-projects/fossicker/test/6x13.lisp")
(fossicker::add-project "~/dev/lisp/local-projects/fossicker/test/test.lisp")
;; (fossicker::remove-project "~/dev/lisp/local-projects/fossicker/test/6x13.lisp")
;; fossicker::*PROJECT-DEFINITIONS*
(fossicker::set-project "test")

(fossicker::generate "meh/bla/test_b_p_.png")
