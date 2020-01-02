(in-package #:glacier)

(defmacro after (amount duration &body body)
  "asynchronously runs BODY after AMOUNT of DURATION"
  (let ((seconds (* amount (cond
			     ((or (eq duration :seconds)
				  (eq duration :second))
			      1)
			     ((or (eq duration :minutes)
				  (eq duration :minute))
			      60)
			     ((or (eq duration :hours)
				  (eq duration :hour))
			      3600)
			     ((or (eq duration :days)
				  (eq duration :day))
			      86400)
			     (t (error "unknown duration"))))))
    `(bt:make-thread
      (lambda ()
	(sleep ,seconds)
	,@body))))

(defun agetf (place indicator &optional default)
  "getf but for alists"
  (or (cdr (assoc indicator place :test #'equal))
      default))
