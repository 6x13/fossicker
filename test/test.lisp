(ql:quickload :fossicker)

(in-package :fcku)

(configure)
(configure :force t)
(configure :force :all)
(configure :system :fossickerrc :force t)
(add-project *config* "~/dev/lisp/local-projects/fossicker/test/6x13.blog")
(add-project *config* "~/dev/lisp/local-projects/fossicker/test/test.project" nil)
(remove-project *config* "~/dev/lisp/local-projects/fossicker/test/6x13.blog")
(remove-project *config* "~/dev/lisp/local-projects/fossicker/test/test.project")
(inspect (fck::projects *config*))
(unload-project "test")
(unload-project "6x13")
(load-project "~/dev/lisp/local-projects/fossicker/test/6x13.blog")
(inspect *project-registry*)
(set-project "test")
(generate "meh/bla/test_b_p_.png")
