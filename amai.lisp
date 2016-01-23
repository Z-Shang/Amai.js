(require 'amai "package")

(in-package #:amai)

(defvar *MACRO-SCOPE* '())

(defconstant *KEYWORDS*
  '("macro"
    "rmacro"))

(defstruct js-macro
  (name "" :type string)
  (arg-lst '() :type list)
  (macrobody "" :type string))

(defstruct parse-state
  (macro-block nil :type boolean)
  (single-quote nil :type boolean)
  (double-quote nil :type boolean)
  (paren-count 0 :type integer)
  (brace-count 0 :type integer)
  (bracket-count 0 :type integer)
  (pointy-count 0 :type integer))

(defun add-macro! (m)
  (setf *MACRO-SCOPE* (cons m *MACRO-SCOPE*)))

(defun and-list (lst)
  (loop for o in lst always (not (null o))))

(defun or-list (lst)
  (loop for o in lst thereis (not (null o))))

(defun gen-arg-lst (m call-arg)
  (let ((orig (js-macro-arg-lst m)))
    (if (< (length call-arg)
           (length orig))
        'ERR-ARITY-DONT-MATCH
        (loop for a in orig for b in call-arg collect (cons a b)))))

(defun compile-macro (m call-arg)
  (let* ((tok (car (tokenize (js-macro-macrobody m))))
         (arg-lst (gen-arg-lst m call-arg))
         (sym-lst (mapcar #'(lambda (s) (cons s (concatenate 'string s (symbol-name (gensym))))) (remove-duplicates (loop for o in tok when (equalp (char o 0) #\`) collect o)))))
    (if (equalp arg-lst 'ERR-ARITY-DONT-MATCH)
        (progn
          (format *error-output* "Macro ~A's arity doesn't match the argument: ~A" (js-macro-name m) call-arg)
          ;;SBCL Only
          (sb-ext:exit))
        (setf tok
              (mapcar #'(lambda (o)
                          (if (equalp (char o 0) #\`)
                              (cdar (member-if #'(lambda (s) (equalp (car s) o)) sym-lst))
                              (if (member-if #'(lambda (s) (equalp (car s) o)) arg-lst)
                                  (cdar (member-if #'(lambda (s) (equalp (car s) o)) arg-lst))
                                  o)))))
        tok)))

(defun tokenize (str states)
  (let ((out '()))
    (loop for c across str
       with tok = ""
       else do (setf tok (concatenate 'string tok (list c)))
       when (or (equalp c #\')
                (equalp c #\"))
       do (if (equalp c #\')
              (setf (parse-state-single-quote states)
                    (not (parse-state-single-quote states)))
              (setf (parse-state-double-quote states)
                    (not (parse-state-double-quote states))))
       when (equalp c #\() do (incf (parse-state-paren-count states))
       when (equalp c #\)) do (decf (parse-state-paren-count states))
       when (equalp c #\[) do (incf (parse-state-brace-count states))
       when (equalp c #\]) do (decf (parse-state-brace-count states))
       when (equalp c #\{) do (incf (parse-state-bracket-count states))
       when (equalp c #\}) do (decf (parse-state-bracket-count states))
       when (equalp c #\<) do (incf (parse-state-pointy-count states))
       when (equalp c #\>) do (decf (parse-state-pointy-count states))
       if (and (or (equalp c #\Space)
                   (equalp c #\Tab)
                   (equalp c #\NewLine))
               (not (or (parse-state-single-quote states)
                        (parse-state-double-quote states))))
       do (progn
            (setf out (append out (list tok)))
            (setf tok "")))
    (cons out states)))

(defun parse-line (fstream ostream states)
  (if (null fstream)
      (progn
        (close fstream)
        (close ostream))
      (let* ((l (read-line fstream))
             (tokens (tokenize l states)))
        (if (or (and-list (mapcar #'(lambda (s) (memer s (car tokens))) *KEYWORDS*))
                (parse-state-macro-block states))
            )
        )
      )
  )

(defun read-file (f &optional outname)
  (parse-line (open f :direction :input :if-does-not-exist nil)
              (open outname :direction :output :if-does-not-exist :create :if-exists :supersede)
              nil))
