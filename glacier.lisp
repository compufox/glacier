;;;; glacier.lisp

(in-package #:glacier)

(defvar *bot*)
(defvar *websocket-client*)

(defmethod run-bot ((bot mastodon-bot))
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

(defun dispatch (message)
  (let* ((parsed (json:decode-json-from-string message))
	 (parsed-payload (json:decode-json-from-string (agetf parsed "payload"))))
    
    (cond
      ((and (string= (agetf parsed "event") "update")
	    (bot-on-update *bot*))
       (funcall (bot-on-update *bot*) 
		(tooter:find-status (bot-client *bot*) (agetf parsed-payload :id))))
      
      ((and (string= (agetf parsed "event") "delete")
	    (bot-on-delete *bot*))
       (funcall (bot-on-delete *bot*) parsed-payload))
      
      ((and (string= (agetf parsed "event") "notification")
	    (bot-on-notification *bot*))
       (funcall (bot-on-notification *bot*) 
		(tooter:find-notification (bot-client *bot*) (agetf parsed-payload :id))))

      (t nil))))
