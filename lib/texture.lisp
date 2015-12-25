(fossicker::register-type 'texture t
                          :regexp
                          '("\\.png$"
                            "\\.jpg$"
                            "\\.tiff$"
                            "\\.tga$")
                          :function #'fossicker::ignore-function)
