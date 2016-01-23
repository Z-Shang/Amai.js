(require 'amai "package")

(in-package #:amai)

(defvar *MACRO-SCOPE* '())

(defconstant *KEYWORDS*
  '("macro"
    "hmacro"
    "rmacro"))

(defstruct js-macro
  name
  arg-lst
  macrobody)

(defstruct parse-state
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

(defun tokenize (str states)
  (let ((out '()))
    (loop for c across str
       with tok = ""
       if (and (or (equalp c #\Space)
                   (equalp c #\Tab)
                   (equalp c #\NewLine))
               (not (or (parse-state-single-quote states)
                        (parse-state-double-quote states))))
       do (progn
            (setf out (append out (list tok)))
            (setf tok ""))
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
       when (equalp c #\>) do (decf (parse-state-pointy-count states)))
    (cons out states)))

(defun parse-line (fstream ostream states)
  (if (null fstream)
      (progn
        (close fstream)
        (close ostream))
      (let ((l (read-line fstream)))
        )
      )
  )

(defun read-file (f &optional outname)
  (parse-line (open f :direction :input :if-does-not-exist nil)
              (open outname :direction :output :if-does-not-exist :create :if-exists :supersede)
              nil))
