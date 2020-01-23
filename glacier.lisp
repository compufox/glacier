;;;; glacier.lisp

(in-package #:glacier)

(defvar *bot*)

(defmacro run-bot (bot &body body)
  "runs BOT, setting up websocket handlers and starting the streaming connection before executing BODY

if BODY is not provided drops into a loop where we sleep until the user quits us, or our connection closes"
  `(handler-case
       (with-user-abort
	 (setf *bot* ,bot)
	 (let ((*websocket-client* (wsd:make-client (format nil "~a/api/v1/streaming?access_token=~a&stream=~a"
							    (get-mastodon-streaming-url)
							    (config :mastodon-token)
							    (config :timeline "user")))))
	   (wsd:on :open *websocket-client* #'print-open)
	   (wsd:on :message *websocket-client* #'dispatch)
	   (wsd:on :close *websocket-client* #'print-close)
	   (wsd:start-connection *websocket-client*)
	
	   ,@(if body
		 body
		 '((loop do (sleep 5)
		        while (eq (wsd:ready-state *websocket-client*) :open))))))
     (user-abort ()
       (format t "shutting down~%"))
     (error (e)
       (format t "encountered unexpected error: ~A~%" e))))

(defun dispatch (message)
  "gets the type of MESSAGE we intercepted and calls the appropriate functions on our bot with the proper tooter object"
  (let* ((parsed (json:decode-json-from-string message))
	 (parsed-payload (json:decode-json-from-string (agetf parsed :payload))))
    
    (cond
      ((and (string= (agetf parsed :event) "update")
	    (slot-boundp *bot* 'on-update))
       (funcall (bot-on-update *bot*) 
		(tooter:find-status (bot-client *bot*) (agetf parsed-payload :id))))
      
      ((and (string= (agetf parsed :event) "delete")
	    (slot-boundp *bot* 'on-delete))
       (funcall (bot-on-delete *bot*) parsed-payload))
      
      ((and (string= (agetf parsed :event) "notification")
	    (slot-boundp *bot* 'on-notification))
       (funcall (bot-on-notification *bot*) 
		(tooter:find-notification (bot-client *bot*) (agetf parsed-payload :id))))

      (t nil))))
