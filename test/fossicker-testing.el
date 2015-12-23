(add-to-list 'load-path "~/dev/lisp/local-projects/fossicker/")
(require 'fossicker-autoloads)
(add-hook 'emacs-lisp-mode-hook 'fossicker-mode)

(add-to-list 'fossicker-projects
             '("6x13"
               (root . "~/dev/lisp/local-projects/fossicker/")
               (path . "Resources/")
               (spec . ((texture "textures/"
                                 (("ldpi/" . 32)
                                  ("mdpi/" . 64)
                                  ("hdpi/" . 128)
                                  ("retina/" . 256)))))))

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

;;; "test.png"
;;; 'test.png'
;;; "test_b_p_n_e.png"
;;; "game/ui/test_b_p_n_e.png"
;;; "game/ui/test_b_p_n_e.vert"
;;; "game/ui/test_b_p_n_e.VERT"
;;; "game/test_b_p_n_e.3ds"
