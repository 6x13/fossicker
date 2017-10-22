;; CC=gcc sbcl --script build.lisp

(in-package :cl-user)
(require "asdf")
#-quicklisp
(let ((quicklisp-init (merge-pathnames "dev/lisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(ql:quickload :fossicker)
(net.didierverna.clon:nickname-package)

(clon:defsynopsis ()
  (text :contents "The Common Lisp server for Thrift's cross-language test suite.")
  (group (:header "Allowed options:")
    (flag :short-name "h" :long-name "help"
          :description "Print this help and exit.")
    (stropt :long-name "port"
	    :description "Number of the port to listen for connections on."
	    :default-value "9090"
	    :argument-name "ARG"
	    :argument-type :optional)
    (stropt :long-name "server-type"
	    :description "The type of server, currently only \"simple\" is available."
	    :default-value "simple"
	    :argument-name "ARG")
    (stropt :long-name "transport"
	    :description "Transport: currently only \"buffered\""
	    :default-value "buffered"
	    :argument-name "ARG")
    (stropt :long-name "protocol"
	    :description "Protocol: currently only \"binary\""
	    :default-value "binary"
	    :argument-name "ARG")))

(defun main ()
  "Entry point for standalone Fossicker server."
  (clon:make-context)
  (when (clon:getopt :short-name "h")
    (clon:help)
    (clon:exit))
  (let ((port "9090"))
    (clon:do-cmdline-options (option name value source)
      (print (list option name value source))
      (if (string= name "port")
  	  (setf port value)))
    (terpri)
	(fossicker:initialize)
    (terpri)
    ;; (thrift:serve (puri:parse-uri (concatenate 'string
  	;; 				       "thrift://127.0.0.1:"
  	;; 				       port))
  	;; 	  thrift.test:thrift-test)
	)
  (clon:exit))

(clon:dump "FossickerServer" main)
