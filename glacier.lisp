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
  "gets the type of MESSAGE we received and calls the appropriate functions on our bot with the proper tooter object"
  (let* ((parsed (json:decode-json-from-string message))
	 (parsed-payload (json:decode-json-from-string (agetf parsed :payload))))
    
    (cond
      ((and (string= (agetf parsed :event) "update")
	    (slot-boundp *bot* 'on-update))
       (funcall (bot-on-update *bot*) 
		(tooter:find-status (bot-client *bot*) (parse-integer (agetf parsed-payload :id)))))
      
      ((and (string= (agetf parsed :event) "delete")
	    (slot-boundp *bot* 'on-delete))
       (funcall (bot-on-delete *bot*) parsed-payload))
      
      ((string= (agetf parsed :event) "notification")

       ;; we go ahead and get the notification object through tooter
       ;;  for ease of parsing, plus we were gonna get it anyway so
       ;;   :shrug:
       (let ((notif (tooter:find-notification (bot-client *bot*) (parse-integer (agetf parsed-payload :id)))))
	 (if (and
	      ;; just some trickery to ensure that if we get a mention, to run
	      ;;  our command dispatch.
	      (not (and (eq (tooter:kind notif) :mention)
			(command-dispatch (tooter:status notif))))

	      ;; if we actually have a notification handler
	      (slot-boundp *bot* 'on-notification))
	     
	     ;; run it
	     (funcall (bot-on-notification *bot*) notif))))

      (t nil))))

(defun command-dispatch (status)
  "parses STATUS content for a command word, and runs any function it has in *commands* with it as the argument"
  (let* ((command (find-if #'commandp
			   (str:words
			    (tooter:plain-format-html (tooter:content status)))))
	 (cmd-func (gethash (subseq command 1) *commands*)))
    (when cmd-func
      (funcall cmd-func status)
      
      ;; if we've hit here we return t, just so the calling function knows
      ;;  that we actually did something
      t)))
