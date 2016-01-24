(require 'amai "package")
(require 'amai-core "amai")
(require 'amai-fe "fe")

(in-package #:amai)

(mapcar #'(lambda (s)
            (format *standard-output* "~A~%" s))
        (parse-input
         (open-file
          (make-pathname
           :directory
           (sb-posix:getcwd)
           :name "test"
           :type "ajs"))
         (Make-parse-state)))
