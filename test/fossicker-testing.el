(add-to-list 'load-path "~/dev/lisp/local-projects/fossicker/")
(require 'fossicker-autoloads)
(add-hook 'emacs-lisp-mode-hook 'fossicker-mode)

(add-to-list 'fossicker-projects
             '("6x13"
               (root . "~/dev/6x13/")
               (path . "Resources/")
               (spec . ((texture "textures/"
                                 (("ldpi/" . 32)
                                  ("mdpi/" . 64)
                                  ("hdpi/" . 128)
                                  ("retina/" . 256)))))))
(set-default-font "6x13")
(setq fossicker-libs '(fossicker-texture fossicker-shader))
(setq fossicker-libs '(fossicker-all))
(setq fossicker-libs '(fossicker-texture fossicker-all))
(fossicker-load-libs 'fossicker-all)
(fossicker-load-libs)

(add-to-list 'fossicker-projects
             '("test"
               (root . "~/dev/test/")
               (path . "Resources/")
               (spec . ((texture "textures/"
                                 (("ldpi/" . 32)
                                  ("mdpi/" . 64)
                                  ("hdpi/" . 128)
                                  ("retina/" . 256)))
                        (model "models/")
                        (bla "meh")
                        (shader "shaders/")))))

(fossicker-register-type 'texture t
                   :regexp
                   '("\\.png\\'"
                     "\\.tga\\'")
                   :function 'fossicker-texture-handler
                   :widgets
                   '((repeat :tag "Sizes"
                             :value (("ldpi/" . 32)
                                     ("hdpi/" . 128))
                             (cons :format "%v"
                                   (directory :tag "Path")
                                   (integer :tag "Unit Size")))))

;;; "test.png"
;;; 'test.png'
;;; "test_b_p_n_e.png"
;;; "game/ui/test_b_p_n_e.png"
;;; "game/ui/test_b_p_n_e.vert"
;;; "game/ui/test_b_p_n_e.VERT"
;;; "game/test_b_p_n_e.3ds"
