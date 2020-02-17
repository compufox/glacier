;;;; glacier.lisp

(in-package #:glacier)

(defvar *bot*)
(defvar *websocket-client*)

(defmacro run-bot ((bot &key delete-command (with-websocket t)) &body body)
  "runs BOT, setting up websocket handlers and starting the streaming connection before executing BODY

if DELETE-COMMAND is non-nil, automatically adds a delete command
if WITH-WEBSOCKET is non-nil (default), automatically starts up a websocket listener for realtime updates

NOTE: DELETE-COMMAND is ignored used if WITH-WEBSOCKET is nil

if BODY is not provided drops into a loop where we sleep until the user quits us, or our connection closes. this functionality does not happen if WITH-WEBSOCKET is nil."
  `(progn
     (setf *bot* ,bot)

     (when ,with-websocket
       (setf *websocket-client* (wsd:make-client
				 (format nil "~a/api/v1/streaming?access_token=~a&stream=~a"
					 (get-mastodon-streaming-url)
					 (config :mastodon-token)
					 (config :timeline "user"))))

       ;; so the bot owner can have the bot delete a post easily, by default
       ;; this option is only used if we're using the websocket client
       (when ,delete-command
	 (add-command "delete" #'delete-parent :privileged t))
       
       (wsd:on :open *websocket-client* #'print-open)
       (wsd:on :message *websocket-client* #'dispatch)
       (wsd:on :close *websocket-client* #'print-close)
       (wsd:start-connection *websocket-client*))
       
     ,@(if body
	   body
	   
	   ;; only drop into this loop when we are using a websocket
	   (when with-websocket
	     '((loop do (sleep 5)
		  while (eq (wsd:ready-state *websocket-client*) :open)))))

     ;; remove any listeners once the provided code is finished executing;p
     (when *websocket-client*
       (terminate-connection))))

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
	      (not (and (mention-p notif)
			(command-dispatch (tooter:status notif))))

	      ;; if we actually have a notification handler
	      (slot-boundp *bot* 'on-notification))
	     
	     ;; run it
	     (funcall (bot-on-notification *bot*) notif))))

      (t nil))))

(defun command-dispatch (status)
  "parses STATUS content for a command word, and runs any function it has in *commands* with it as the argument

if STATUS comes from an account the bot is following, also checks for any command in *privileged-commands*"
  (let ((command (find-if #'commandp
			  (str:words
			   (tooter:plain-format-html (tooter:content status))))))
	 
    (when command

      ;; if the bot is following the account that STATUS came from,
      ;;  we search for both a privileged AND a normal command
      (let ((cmd-func (or (and (privileged-reply-p status)
			       (gethash command *privileged-commands*))
			  (gethash command *commands*))))
	(when cmd-func
	  (funcall cmd-func status)

	  ;; if we've hit here we return t, just so the calling function knows
	  ;;  that we actually did something
	  t)))))


(defun terminate-connection ()
  "closes the websocket connection and clears the variable from memory"
  (wsd:remove-all-listeners *websocket-client*)
  (setf *websocket-client* nil))
