(ql:quickload :fossicker)
(fossicker::add-project "~/dev/lisp/local-projects/fossicker/test/6x13.lisp")
(fossicker::add-project "~/dev/lisp/local-projects/fossicker/test/test.lisp")
(fossicker::remove-project "~/dev/lisp/local-projects/fossicker/test/6x13.lisp")
(fossicker::load-projects)
fossicker::*PROJECT-DEFINITIONS*
(fossicker::set-project "test")
(fossicker::generate "test.png")

(fossicker::register-type 'shader t
                          :regexp '("\\.vert\\'"
                                    "\\.frag\\'"
                                    "\\.tesc\\'"
                                    "\\.tese\\'"
                                    "\\.geom\\'"
                                    "\\.comp\\'")
                          :formats (lambda (ext)
                                     (cond ((member-ignore-case
                                             ext
                                             '("vert" "vrt")) '("vrt" "vert"))
                                           ((member-ignore-case
                                             ext
                                             '("frag" "frg")) '("frag" "frg"))
                                           ((member-ignore-case
                                             ext
                                             '("tese" "tes")) '("tese" "tes"))
                                           (t nil))))

(fossicker::register-type 'texture t
                          :regexp
                          '("\\.png$"
                            "\\.jpg$"
                            "\\.tiff$"
                            "\\.tga$")
                          :function #'fossicker::ignore-function)

(setq fossicker::*data-path* "~/dev/lisp/local-projects/fossicker/data/")

