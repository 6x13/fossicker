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
