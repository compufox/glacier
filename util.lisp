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

(defun get-mastodon-streaming-url ()
  "gets the websocket url for the mastodon instance"
  (handler-case
      (agetf
       (agetf (json:decode-json-from-string
	       (dex:get (format nil "https://~a/api/v1/instance"
				(config :mastodon-instance))))
	      :urls)
       :streaming--api)
    (error (e) (error "unexpected error occurred"))))

(defun print-open ()
  "prints a message when the websocket is connected"
  (print "websocket connected"))

(defun print-close (&key code reason)
  "prints a message when the websocket is closed"
  (when (and code reason)
    (format t "websocket closed because ~A (code=~A)~%" reason code)))
