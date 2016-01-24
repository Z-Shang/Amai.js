(require 'amai "package")
(require 'amai-core "amai")

(in-package #:amai)

(defun open-file (path)
  (with-open-file (in path :direction :input :if-does-not-exist nil)
    (when in
      (loop for line = (read-line in nil)
         while line collect line))))

(defun lst-to-string (lst)
  (if (null lst)
      ""
      (concatenate 'string (car lst) " " (lst-to-string (cdr lst)))))

(defun get-arg-lst (lst)
  (if (null lst)
      nil
      (cond
        ((equalp (car lst) (string #\)))
         nil)
        ((equalp (car lst) (string #\,))
         (get-arg-lst (cdr lst)))
        (t
         (cons (car lst) (get-arg-lst (cdr lst)))))))

(defun parse-input (lst state)
  (if (null lst)
      nil
      (let* ((tk (tokenize (car lst) state))
             (toks (car tk))
             (new-state (cdr tk)))
        (cond
          ((loop for k in *KEYWORDS* thereis (equalp k (car toks)))
           (progn
             (setf (parse-state-macro-state new-state) t)
             (let ((tm (make-js-macro :name (second toks))))
               (setf (js-macro-arg-lst tm)
                     (get-arg-lst (cdddr toks)))
               (setf (parse-state-tmp-macro new-state) tm)
               (parse-input (cdr lst) new-state))))
          ((parse-state-macro-state new-state)
           (progn
             (if (= 0 (parse-state-bracket-count new-state))
                 (progn
                   (setf (parse-state-macro-state new-state) nil)
                   (add-macro! (parse-state-tmp-macro new-state))
                   (setf (parse-state-tmp-macro new-state)
                         (make-js-macro)))
                 (progn
                   (setf (js-macro-macrobody (parse-state-tmp-macro new-state))
                         (concatenate 'string (js-macro-macrobody (parse-state-tmp-macro new-state)) (lst-to-string toks)))))
             (parse-input (cdr lst) new-state)))
          (t
           (if (loop for m in *MACRO-SCOPE* thereis (member (js-macro-name m) toks :test #'equalp))
               (let ((m (loop for m in *MACRO-SCOPE* when (member (js-macro-name m) toks :test #'equalp) return m)))
                 (labels ((get-macro-call (l)
                            (loop for s in l with p = 0
                               when (equalp s (string #\()) do (incf p)
                               when (equalp s (string #\))) do (decf p)
                               when (and (> p 0) (not (or (equalp s (string #\,))
                                                          (equalp s (string #\())
                                                          (equalp s (string #\))))))
                               collect s))
                          (get-rest (l)
                            (loop for s in l with p = 0
                               when (equalp s (string #\()) do (incf p)
                               when (equalp s (string #\))) do (decf p)
                               when (> p 0)
                               count s)))
                   (cons
                    (compile-macro m (get-macro-call (subseq toks 1)))
                    (parse-input (cdr lst) new-state))))
               (cons toks (parse-input (cdr lst) new-state))))))))

(defun write-to-file (path s)
  (with-open-file (out path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mapcar #'(lambda (o)
                (if (not (null o))
                    (format out "~{~A ~}~%" o))) s)))

(provide 'amai-fe)
