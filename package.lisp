;;Package def
(in-package #:cl)

(defpackage #:amai
  (:use :cl)
  (:export
   run
   write-to-file
   parse-input
   open-file))

(provide 'amai)
