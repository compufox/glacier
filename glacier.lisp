;;;; glacier.lisp

(in-package #:glacier)

(defvar *bot*)
(defvar *websocket-client*)

(defun run-bot (bot)
  (handler-case
      (with-user-abort
	(let ((*bot* bot)
	      (*websocket-client* (wsd:make-client (format nil "~a/api/v1/streaming?access_token=~a&stream=~a"
							   (get-mastodon-streaming-url)
							   (config :mastodon-token)
							   (config :timeline "user")))))
	  (wsd:on :open *websocket-client* #'print-open)
	  (wsd:on :message *websocket-client* #'dispatch)
	  (wsd:on :close *websocket-client* #'print-close)
	  (wsd:start-connection *websocket-client*)
	
	  (loop while (eq (wsd:ready-state *websocket-client*) :open)
	        do (sleep 5))))
    (user-abort ()
      (format t "shutting down~%"))
    (error (e)
      (format t "encountered unexpected error: ~A~%" e))))


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

(defun dispatch (message)
  (let ((parsed (json:decode-json-from-string message)))
    (cond
      ((string= (agetf parsed "event") "update")
       (funcall (bot-on-update *bot*) (agetf parsed "payload")))
      
      ((string= (agetf parsed "event") "delete")
       (funcall (bot-on-delete *bot*) (agetf parsed "payload")))
      
      ((string= (agetf parsed "event") "notification")
       (funcall (bot-on-notification *bot*) (agetf parsed "payload")))

      (t nil))))
