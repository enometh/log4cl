;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <2020-04-05 06:59:13 IST>
;;;   Touched: Sun Apr 05 06:58:56 2020 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2020 Madhu.  All Rights Reserved.
;;;
(in-package #:log4cl)

;;; these functions are from slime
(defun truncate-string (string width &optional ellipsis)
  (let ((len (length string)))
    (cond ((< len width) string)
          (ellipsis (cat (subseq string 0 width) ellipsis))
          (t (subseq string 0 width)))))

(defun call/truncated-output-to-string (length function
                                        &optional (ellipsis ".."))
  "Call FUNCTION with a new stream, return the output written to the stream.
If FUNCTION tries to write more than LENGTH characters, it will be
aborted and return immediately with the output written so far."
  (let ((buffer (make-string (+ length (length ellipsis))))
        (fill-pointer 0))
    (block buffer-full
      (flet ((write-output (string)
               (let* ((free (- length fill-pointer))
                      (count (min free (length string))))
                 (replace buffer string :start1 fill-pointer :end2 count)
                 (incf fill-pointer count)
                 (when (> (length string) free)
                   (replace buffer ellipsis :start1 fill-pointer)
                   (return-from buffer-full buffer)))))
        (let ((stream (make-output-stream #'write-output)))
          (funcall function stream)
          (finish-output stream)
          (subseq buffer 0 fill-pointer))))))

(defmacro with-string-stream ((var &key length bindings)
                              &body body)
  (cond ((and (not bindings) (not length))
         `(with-output-to-string (,var) . ,body))
        ((not bindings)
         `(call/truncated-output-to-string
           ,length (lambda (,var) . ,body)))
        (t
         `(with-bindings ,bindings
            (with-string-stream (,var :length ,length)
              . ,body)))))
