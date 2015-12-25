(ql:quickload :fossicker)
(fossicker::add-project "~/dev/lisp/local-projects/fossicker/test/6x13.lisp")
(fossicker::add-project "~/dev/lisp/local-projects/fossicker/test/test.lisp")
(fossicker::remove-project "~/dev/lisp/local-projects/fossicker/test/6x13.lisp")
(fossicker::load-projects)
fossicker::*PROJECT-DEFINITIONS*
(fossicker::set-project "test")
(fossicker::generate "test.png")

(fossicker::register-type 'shader t
                          :regexp '("\\.vert\$"
                                    "\\.frag\$"
                                    "\\.tesc\$"
                                    "\\.tese\$"
                                    "\\.geom\$"
                                    "\\.comp\$")
                          :formats (lambda (ext)
                                     (cond ((member ext
                                                    '("vert" "vrt")
                                                    :test #'string-equal)
                                            '("vrt" "vert"))
                                           ((member ext
                                                    '("frag" "frg")
                                                    :test #'string-equal)
                                            '("frag" "frg"))
                                           ((member ext
                                                    '("tese" "tes")
                                                    :test #'string-equal)
                                            '("tese" "tes"))
                                           (t nil))))

(fossicker::register-type 'texture t
                          :regexp
                          '("\\.png$"
                            "\\.jpg$"
                            "\\.tiff$"
                            "\\.tga$")
                          :function #'fossicker::ignore-function)

(setq fossicker::*data-path* "~/dev/lisp/local-projects/fossicker/data/")

