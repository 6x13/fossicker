(add-to-list 'load-path "~/dev/lisp/local-projects/fossicker/")
(require 'fossicker-autoloads)
(add-hook 'emacs-lisp-mode-hook 'fossicker-mode)

(setq fossicker-libs '(fossicker-texture fossicker-shader))
(setq fossicker-libs '(fossicker-all fossicker-model))
(fossicker-load-libs 'fossicker-all)

(add-to-list 'fossicker-projects '("~/dev/lisp/local-projects/fossicker/test/6x13.proj"
                                   "~/dev/lisp/local-projects/fossicker/"))
(add-to-list 'fossicker-projects '("~/dev/lisp/local-projects/fossicker/test/test.proj"))

;;; "test.png"
;;; 'test.png'
;;; "test_b_p_n_e.png"
;;; "game/ui/test_b_p_n_e.png"
;;; "game/ui/test_b_p_n_e.vert"
;;; "game/ui/test_b_p_n_e.VERT"
;;; "game/test_b_p_n_e.3ds"

