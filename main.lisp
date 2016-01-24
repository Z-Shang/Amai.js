(require 'amai "package")
(require 'amai-core "amai")
(require 'amai-fe "fe")

(in-package #:amai)

(declaim (optimize
          (speed 3)
          (space 0)
          (safety 0)
          (debug 0)))

(defun run ()
  (cond
    ((= 2 (length sb-ext:*posix-argv*))
     (write-to-file
      (make-pathname :directory (sb-posix:getcwd) :name "out" :type "js")
      (parse-input (open-file
                    (make-pathname :directory (sb-posix:getcwd) :name (second sb-ext:*posix-argv*)))
                   (make-parse-state))))
    ((= 3 (length sb-ext:*posix-argv*))
     (write-to-file
      (make-pathname :directory (sb-posix:getcwd) :name (third sb-ext:*posix-argv*) :type "js")
      (parse-input (open-file (make-pathname :directory (sb-posix:getcwd) :name (second sb-ext:*posix-argv*)))
                   (make-parse-state))))))

(sb-ext:save-lisp-and-die "ajs" :executable t :toplevel #'run)
