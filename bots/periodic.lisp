;;; periodic.lisp
;;

(in-package #:glacier)

(defclass periodic-bot (mastodon-bot)
  ((duration :initarg :duration
	     :accessor duration)
   (units :initarg :units
	  :accessor units)))

(defun make-periodic-bot (duration units &rest args &key &allow-other-keys)
  (let ((bot (apply #'make-instance `(periodic-bot ,@args))))
    (setf (bot-duration bot) duration
	  (bot-units bot) units)))
