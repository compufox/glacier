(in-package #:glacier)

(defun parse-time (amount duration)
  "parses AMOUNT of DURATION into seconds"
  (* amount (cond
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
	     (t (error "unknown duration")))))

(defmacro after (amount duration &body body)
  "asynchronously runs BODY after AMOUNT of DURATION"
  `(bt:make-thread
    (lambda ()
      (sleep (parse-time ,amount ,duration))
      ,@body)))

(defmacro after-every (amount duration &body body)
  "runs BODY after every AMOUNT of DURATION"
  `(loop do (sleep (parse-time ,amount ,duration))
	    ,@body))

(defun agetf (place indicator &optional default)
  "getf but for alists"
  (or (cdr (assoc indicator place :test #'equal))
      default))

;; probably change this to use tooter client obj
(defun get-mastodon-streaming-url ()
  "gets the websocket url for the mastodon instance"
  (handler-case
      (agetf
       (agetf (json:decode-json-from-string
	       (dex:get (format nil "~a/api/v1/instance"
				(add-scheme (config :mastodon-instance)))))
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

(defun add-scheme (domain)
  "adds https scheme to DOMAIN if it isnt already there"
  (if (search "https://" domain)
      domain
      (concatenate 'string
		   "https://"
		   domain)))

(defun commandp (word)
  "checks if WORD is a command"
  (str:starts-with-p "!" word))

(defun add-command (cmd function)
  "adds a command into our hash

CMD should be a string
FUNCTION should be a function that accepts a single parameter (a tooter:status object)"
  (setf (gethash cmd *commands*) function))
