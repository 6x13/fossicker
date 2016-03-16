;;;; interaction.lisp

(defpackage #:interaction
  (:use #:cl)
  (:export
   #:compile-interaction))

(in-package #:interaction)

#|
or: selection box, shown in parallel, pick one then process.
and: first presentation found will be used.
member: combobox
bit: checkbox

string: lineedit

simple-base-string
base-string
simple-string

BIT RELATED:
bit
bit-vector
simple-bit-vector

FUNCTION RELATED:
function
generic-function

PATHNAME RELATED:
logical-pathname
pathname

package

SYMBOL RELATED:
keyword
symbol

SEQUENCE RELATED:
array
sequence
simple-array
simple-vector
vector
|#

(defvar *default-numerical-range* 100)

(defvar *default-numerical-lower-limit* (- 0 (/ *default-numerical-range* 2)))
(defvar *default-numerical-upper-limit* (- 0 (/ *default-numerical-range* 2)))

(defvar *default-real-lower-limit* *default-numerical-lower-limit*)
(defvar *default-real-upper-limit* *default-numerical-upper-limit*)

(defvar *default-rational-lower-limit* *default-numerical-lower-limit*)
(defvar *default-rational-upper-limit* *default-numerical-upper-limit*)

(defvar *default-integer-lower-limit* *default-numerical-lower-limit*)
(defvar *default-integer-upper-limit* *default-numerical-upper-limit*)

(defvar *default-float-lower-limit* *default-numerical-lower-limit*)
(defvar *default-float-upper-limit* *default-numerical-upper-limit*)

(defun unspecified-p (x)
  (eql x '*))

(defun default (x default)
  "When  a  type-specifier that  expects  a  type-specifier in  its  subsidiary
  informartion can implicitly  or explicitly accept * as its  argument, it will
  return provided DEFAULT."
  (if (unspecified-p x)
      default
      x))

(define-condition ambiguity (warning)
  ((type-specifier
    :initarg :type-specifier
    :reader ambiguity-type-specifier)
   (subsidiary
    :type list
    :initform nil
    :initarg :subsidiary
    :reader ambiguity-subsidiary))
  (:report (lambda (condition stream)
             (format stream "Problem resolving type-specifier (~A~{ ~A~})."
                     (ambiguity-type-specifier condition)
                     (ambiguity-subsidiary condition)))))

(defun call-with-using-value (value function)
  (handler-bind ((ambiguity (lambda (c)
                              (declare (ignore c))
                              (use-value value))))
    (funcall function)))

(defmacro with-using-value ((form) &body body)
  "Wrapper macro for forwarding output to LOG-STREAM of WIDGET."
  `(call-with-using-value ,form (lambda () ,@body)))

(defgeneric compile-interaction (type-specifier stream
                                 &key subsidiary
                                 &allow-other-keys)
  (:documentation ""))

(defmethod compile-interaction (type-specifier
                                stream
                                &key subsidiary)
  (declare (ignore stream))
  (restart-case
      (warn 'ambiguity :type-specifier type-specifier
                       :subsidiary subsidiary)
    (use-value (value) value)))

(defmethod compile-interaction ((type-specifier null)
                                stream
                                &key)
  (declare (ignore stream))
  ;; "Description: The type  nil contains no objects and so  is also called the
  ;; empty type. The type nil is a subtype  of every type. No object is of type
  ;; nil.
  ;;
  ;; Notes: The type containing  the object nil is the type  null, not the type
  ;; nil." X3J13/94-101R (Type NIL)
  nil)

(defmethod compile-interaction ((compound cons)
                                stream
                                &rest args)
  "Compound type specifier."
  ;; "If a type specifier  is a list, the car of the list  is a symbol, and the
  ;; rest of the list is subsidiary  type information. Such a type specifier is
  ;; called a compound type specifier.   Except as explicitly stated otherwise,
  ;; the subsidiary items can be  unspecified. The unspecified subsidiary items
  ;; are  indicated by  writing  *."  X3J13/94-101R  (Type Specifiers,  section
  ;; 4.2.3)
  (destructuring-bind (name &rest subsidiary) compound
    (check-type name symbol)
    (check-type subsidiary list)
    (apply #'compile-interaction name stream :subsidiary subsidiary args)))

;;
;;;; Type Specifiers
;;
;;

(defmethod compile-interaction ((type-specifier (eql 't))
                                stream
                                &key)
  (declare (ignore type-specifier
                   stream))
  ;; "The  set of  all  objects. The  type  t  is a  supertype  of every  type,
  ;; including itself.  Every object is of type t." X3J13/94-101R (System Class
  ;; T)
  t)

(defmethod compile-interaction ((type-specifier (eql 'null))
                                stream
                                &key)
  (declare (ignore stream))
  ;; "The only object of type null is  nil, which represents the empty list and
  ;; can also be notated ()." X3J13/94-101R (System Class NULL)
  t)

(defmethod compile-interaction ((type-specifier (eql 'atom))
                                stream
                                &key)
  ;; "It is equivalent to (not cons)." X3J13/94-101R (Type ATOM)
  
  ;; For now we delegate decision about  ATOM type to NOT, which simply returns
  ;; T.
  (compile-interaction 'not stream :subsidiary '(cons)))


(defmethod compile-interaction ((name (eql 'cons))
                                stream
                                &key subsidiary)
  ;; "This denotes  the set of  conses whose car is  constrained to be  of type
  ;; car-typespec and whose cdr is constrained  to be of type cdr-typespec. (If
  ;; either car-typespec or cdr-typespec is *, it  is as if the type t had been
  ;; denoted.)" X3J13/94-101R (System Class CONS)
  (destructuring-bind (&optional
                         (car-typespec '*)
                         (cdr-typespec '*)
                       &aux
                         (car-interaction (compile-interaction (default
                                                                car-typespec t)
                                                               stream))
                         (cdr-interaction (compile-interaction (default
                                                                cdr-typespec t)
                                                               stream)))
      subsidiary
    (list :cons car-interaction cdr-interaction)))


(defmethod compile-interaction ((name (eql 'eql))
                                stream
                                &key subsidiary)
  ;; "The argument  object is  required.  The  object can  be *,  but if  so it
  ;; denotes itself (the symbol *) and does not represent an unspecified value.
  ;; The symbol eql  is not valid as an atomic  type specifier."  X3J13/94-101R
  ;; (Type Specifier EQL)
  (destructuring-bind (object) subsidiary
    (list :eql object)))

(defmethod compile-interaction ((name (eql 'member))
                                stream
                                &key ((:subsidiary objects)))
  ;; "The type specifiers  (member) and nil are equivalent. *  can be among the
  ;; objects, but if so it denotes itself (the symbol *) and does not represent
  ;; an unspecified value. The symbol member  is not valid as a type specifier;
  ;; and,  specifically, it  is  not  an abbreviation  for  either (member)  or
  ;; (member *)." X3J13/94-101R (Type Specifier MEMBER)
  (if objects
      (apply #'list :member objects)
      (compile-interaction nil stream)))

(defun ignoring-compile-each-interaction (typespecs stream ignored)
  (loop for typespec in typespecs
        as interaction = (compile-interaction typespec stream)
        unless (eql ignored interaction)
          collect interaction into interactions of-type list
          finally (return interactions)))

(defmethod compile-interaction ((name (eql 'or))
                                stream
                                &key ((:subsidiary typespecs))
                                &aux ambiguity
                                  (interactions
                                   (handler-bind ((ambiguity
                                                    (lambda (c)
                                                      (declare (ignore c))
                                                      (setf ambiguity t)
                                                      (use-value nil))))
                                     (ignoring-compile-each-interaction
                                      typespecs stream nil))))
  ;; "This denotes the set  of all objects of the type  determined by the union
  ;; of the typespecs." X3J13/94-101R (Type Specifier OR)
  
  ;; "* is not permitted as an argument.
  ;;
  ;; The type  specifiers (or) and  nil are equivalent.   The symbol or  is not
  ;; valid as  a type specifier; and,  specifically, it is not  an abbreviation
  ;; for (or). " X3J13/94-101R (Type Specifier OR)
  (case (length interactions)
    (0 (if ambiguity
           (call-next-method)
           (compile-interaction nil stream)))
    (1 (car interactions))
    (t (apply #'list :or interactions))))

(defmethod compile-interaction ((name (eql 'and))
                                stream
                                &key ((:subsidiary typespecs))
                                &aux ambiguity
                                  (interactions
                                   (handler-bind ((ambiguity
                                                    (lambda (c)
                                                      (declare (ignore c))
                                                      (setf ambiguity t)
                                                      (use-value t))))
                                     (ignoring-compile-each-interaction
                                      typespecs stream t))))
  ;; "This  denotes the  set  of all  objects  of the  type  determined by  the
  ;; intersection of the typespecs.
  ;;
  ;; * is not permitted as an argument.
  ;;
  ;; The type  specifiers (and)  and t  are equivalent. The  symbol and  is not
  ;; valid as  a type specifier, and,  specifically, it is not  an abbreviation
  ;; for (and)." X3J13/94-101R (Type Specifier AND)
  (case (length interactions)
    (0 (if ambiguity
           (call-next-method)
           (compile-interaction t stream)))
    (1 (car interactions))
    (t (apply #'list :and interactions))))


(defmethod compile-interaction ((name (eql 'satisfies))
                                stream
                                &key subsidiary)
  ;; "This  denotes  the  set  of   all  objects  that  satisfy  the  predicate
  ;; predicate-name, which must be a symbol whose global function definition is
  ;; a one-argument  predicate. A name  is required for  predicate-name; lambda
  ;; expressions are not allowed. For  example, the type specifier (and integer
  ;; (satisfies evenp)) denotes the set of all even integers. The form (typep x
  ;; '(satisfies p)) is equivalent to (if (p x) t nil).
  ;;
  ;; The argument is required. The symbol * can be the argument, but it denotes
  ;; itself (the symbol *), and does not represent an unspecified value.
  ;;
  ;; The  symbol satisfies  is not  valid as  a type  specifier." X3J13/94-101R
  ;; (Type Specifier SATISFIES)
  (destructuring-bind (predicate-name) subsidiary
    (check-type predicate-name symbol)
    ;; We cannot infer type characteristics  from a SATISFIES type-specifier in
    ;; an efficient manner. We simply ignore this type specifier and return T.
    (call-next-method)))

(defmethod compile-interaction ((name (eql 'not))
                                stream
                                &key subsidiary)
  ;; "This denotes the set of all objects that are not of the type typespec.
  ;;
  ;; The argument is required, and cannot be *.
  ;;
  ;; The symbol  not is  not valid  as a  type specifier."  X3J13/94-101R (Type
  ;; Specifier NOT)
  (destructuring-bind (typespec) subsidiary
    (declare (ignore typespec))
    ;; We cannot  infer type  characteristics from a  NOT type-specifier  in an
    ;; efficient manner. We simply ignore this type specifier and return T.
    (call-next-method)))

(defmethod compile-interaction ((name (eql 'mod))
                                stream
                                &key subsidiary)
  ;; "This  denotes the  set  of non-negative  integers less  than  n. This  is
  ;; equivalent to (integer 0 (n)) or to (integer 0 m), where m=n-1.
  ;;
  ;; The argument is required, and  cannot be *." X3J13/94-101R (Type Specifier
  ;; MOD)
  (destructuring-bind (n) subsidiary
    ;; We resolve the presentation of MOD by means of presentation of INTEGER.
    (compile-interaction 'integer stream :subsidiary (list (1- n)))))

;;
;;;; Number
;;
;;

(defmethod compile-interaction ((type-specifier (eql 'number))
                                stream
                                &key)
  ;; "The type  number contains  objects which represent  mathematical numbers.
  ;; The   types  real   and  complex   are  disjoint   subtypes  of   number."
  ;; X3J13/94-101R (System Class NUMBER)
  t)

;;
;;;;; Real
;;
;;

(defmethod compile-interaction ((name (eql 'real))
                                stream
                                &key subsidiary)
  ;; "lower-limit,  upper-limit---interval  designators  for  type  real.   The
  ;; defaults  for  each of  lower-limit  and  upper-limit  is the  symbol  *."
  ;; X3J13/94-101R (System Class REAL)
  (destructuring-bind (&optional (lower-limit '*) (upper-limit '*)
                       &aux
                         (defaulted-lower-limit
                          (default lower-limit
                                   *default-real-lower-limit*))
                         (defaulted-upper-limit
                          (default upper-limit
                                   *default-real-upper-limit*))) subsidiary
    ;; "This denotes  the reals  on the interval  described by  lower-limit and
    ;; upper-limit." X3J13/94-101R (System Class REAL)
    (list :real :between defaulted-lower-limit :and defaulted-upper-limit)))

;;
;;;;; Complex
;;
;;

(defmethod compile-interaction ((name (eql 'complex))
                                stream
                                &key subsidiary)
  ;; "Every element  of this type  is a complex  whose real part  and imaginary
  ;; part are  each of  type (upgraded-complex-part-type typespec).   This type
  ;; encompasses  those complexes  that can  result by  giving numbers  of type
  ;; typespec to complex." X3J13/94-101R (System Class COMPLEX)
  (destructuring-bind (&optional (typespec '*)
                       &aux
                         (defaulted-typespec (default typespec 'real))
                         ;; (upgraded (upgraded-complex-part-type
                         ;;            defaulted-typespec))
                         (interaction (compile-interaction defaulted-typespec
                                                           stream))) subsidiary
    ;; "typespec---a  type specifier  that  denotes a  subtype  of type  real."
    ;; X3J13/94-101R (System Class COMPLEX)
    ;;
    ;; "(complex type-specifier) refers  to all complexes that  can result from
    ;; giving numbers of type type-specifier  to the function complex, plus all
    ;; other complexes  of the same specialized  representation." X3J13/94-101R
    ;; (System Class COMPLEX)
    (list :complex interaction)))

;;
;;;;; Rational
;;
;;

(defmethod compile-interaction ((name (eql 'rational))
                                stream
                                &key subsidiary)
  ;; "lower-limit, upper-limit---interval  designators for type  rational.  The
  ;; defaults  for  each of  lower-limit  and  upper-limit  is the  symbol  *."
  ;; X3J13/94-101R (System Class RATIONAL)
  (destructuring-bind (&optional (lower-limit '*) (upper-limit '*)
                       &aux
                         (defaulted-lower-limit
                          (default lower-limit
                                   *default-rational-lower-limit*))
                         (defaulted-upper-limit
                          (default upper-limit
                                   *default-rational-upper-limit*))) subsidiary
    ;; "This denotes the rationals on the interval described by lower-limit and
    ;; upper-limit." X3J13/94-101R (System Class RATIONAL)
    (list :rational :between defaulted-lower-limit :and defaulted-upper-limit)))

(defmethod compile-interaction ((type-specifier (eql 'ratio))
                                stream
                                &key)
  ;; "A ratio is  a number representing the mathematical ratio  of two non-zero
  ;; integers, the numerator and denominator,  whose greatest common divisor is
  ;; one,  and of  which the  denominator is  positive and  greater than  one."
  ;; X3J13/94-101R (System Class RATIO)
  (compile-interaction 'rational stream))

;;
;;;;; Integer
;;
;;

(defmethod compile-interaction ((name (eql 'integer))
                                stream
                                &key subsidiary)
  ;; "lower-limit,  upper-limit---interval designators  for type  integer.  The
  ;; defaults  for  each of  lower-limit  and  upper-limit  is the  symbol  *."
  ;; X3J13/94-101R (System Class INTEGER)
  (destructuring-bind (&optional (lower-limit '*) (upper-limit '*)
                       &aux
                         (defaulted-lower-limit
                          (default lower-limit
                                   *default-integer-lower-limit*))
                         (defaulted-upper-limit
                          (default upper-limit
                                   *default-integer-upper-limit*))) subsidiary
    ;; "This denotes the integers on  the interval described by lower-limit and
    ;; upper-limit." X3J13/94-101R (System Class INTEGER)
    (list :integer :between defaulted-lower-limit :and defaulted-upper-limit)))

(defmethod compile-interaction ((type-specifier (eql 'fixnum))
                                stream
                                &key)
  ;; "A fixnum  is an integer  whose value is between  most-negative-fixnum and
  ;; most-positive-fixnum inclusive." X3J13/94-101R (Type FIXNUM)
  (compile-interaction 'integer stream :subsidiary (list most-negative-fixnum
                                                         most-positive-fixnum)))

(defmethod compile-interaction ((type-specifier (eql 'bignum))
                                stream
                                &key)
  ;; "The type  bignum is defined  to be  exactly (and integer  (not fixnum))."
  ;; X3J13/94-101R (Type BIGNUM)
  (compile-interaction 'and stream :subsidiary '(integer (not fixnum))))

(defmethod compile-interaction ((name (eql 'unsigned-byte))
                                stream
                                &key subsidiary)
  ;; "This denotes the set of non-negative  integers that can be represented in
  ;; a byte of  size s (bits). This is  equivalent to (mod m) for  m=2^s, or to
  ;; (integer  0  n)   for  n=2^s-1.   The  type  unsigned-byte   or  the  type
  ;; (unsigned-byte  *) is  the same  as the  type (integer  0 *),  the set  of
  ;; non-negative integers." X3J13/94-101R (Type UNSIGNED-BYTE)
  (destructuring-bind (&optional (s '*)) subsidiary
    (compile-interaction 'integer stream
                         :subsidiary (list 0 (if (unspecified-p s)
                                                 s
                                                 (1- (expt 2 s)))))))

(defmethod compile-interaction ((name (eql 'signed-byte))
                                stream
                                &key subsidiary)
  ;; "This  denotes   the  set   of  integers  that   can  be   represented  in
  ;; two's-complement form in a byte of  s bits. This is equivalent to (integer
  ;; -2^s-1 2^s-1-1). The  type signed-byte or the type (signed-byte  *) is the
  ;; same as the type integer. " X3J13/94-101R (Type SIGNED-BYTE)
  (destructuring-bind (&optional (s '*)) subsidiary
    (compile-interaction 'integer stream
                         :subsidiary (if (unspecified-p s)
                                         '(* *)
                                         (list (- (expt 2 (1- s)))
                                               (1- (expt 2 (1- s))))))))

;;
;;;;; Float
;;
;;

(defmethod compile-interaction ((name (eql 'float))
                                stream
                                &key subsidiary)
  ;; "lower-limit,  upper-limit---interval  designators  for type  flaot.   The
  ;; defaults  for  each of  lower-limit  and  upper-limit  is the  symbol  *."
  ;; X3J13/94-101R (System Class FLOAT)
  (destructuring-bind (&optional (lower-limit '*) (upper-limit '*)
                       &aux
                         (defaulted-lower-limit
                          (default lower-limit
                                   *default-float-lower-limit*))
                         (defaulted-upper-limit
                          (default upper-limit
                                   *default-float-upper-limit*))) subsidiary
    ;; "This denotes  the floats on  the interval described by  lower-limit and
    ;; upper-limit." X3J13/94-101R (System Class FLOAT)
    (list :float :between defaulted-lower-limit :and defaulted-upper-limit)))

(defmethod compile-interaction ((name (eql 'short-float))
                                stream
                                &key subsidiary)
  (compile-interaction 'float stream :subsidiary subsidiary))

(defmethod compile-interaction ((name (eql 'single-float))
                                stream
                                &key subsidiary)
  (compile-interaction 'float stream :subsidiary subsidiary))

(defmethod compile-interaction ((name (eql 'double-float))
                                stream
                                &key subsidiary)
  (compile-interaction 'float stream :subsidiary subsidiary))

(defmethod compile-interaction ((name (eql 'long-float))
                                stream
                                &key subsidiary)
  (compile-interaction 'float stream :subsidiary subsidiary))

;;
;;;; Character
;;
;;

(defmethod compile-interaction ((name (eql 'character))
                                stream
                                &key)
  ;; "The types base-char and extended-char form an exhaustive partition of the
  ;; type character." X3J13/94-101R (System Class CHARACTER)
  :char)

(defmethod compile-interaction ((name (eql 'standard-character))
                                stream
                                &key)
  (compile-interaction 'character stream))

(defmethod compile-interaction ((name (eql 'base-char))
                                stream
                                &key)
  (compile-interaction 'character stream))

(defmethod compile-interaction ((name (eql 'extended-char))
                                stream
                                &key)
  ;; "The  type extended-char  is equivalent  to the  type (and  character (not
  ;; base-char))." X3J13/94-101R (Type EXTENDED-CHAR)
  (compile-interaction 'and stream :subsidiary '(character (not base-char))))
