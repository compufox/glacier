(in-package #:glacier)

(defvar *commands* (make-hash-table :test #'equal)
  "hash table containing the bot's commands

KEY is the command as a string
VALUE is a function that accepts a tooter:status object as a parameter")

(defclass bot-client (tooter:client) ()
  (:default-initargs
   :name "GlacierBot"
   :website "https://github.com/compufox/glacier"))

(defclass mastodon-bot ()
  ((on-update :initarg :on-update
	      :accessor bot-on-update)
   (on-delete :initarg :on-delete
	      :accessor bot-on-delete)
   (on-notification :initarg :on-notification
		    :accessor bot-on-notification)
   (account-id :reader bot-account-id)
   (client :reader bot-client))
  (:documentation "bot superclass"))

(defmethod initialize-instance :after ((instance mastodon-bot) &rest initargs
				       &key config-file &allow-other-keys)
  (load-config config-file)
  (let ((client (make-instance 'bot-client
			       :access-token (config :mastodon-token)
			       :base (add-scheme (config :mastodon-instance)))))
    (setf (slot-value instance 'client) client
	  (slot-value instance 'account-id) (tooter:id (tooter:verify-credentials client)))))

(defun commandp (word)
  "checks if WORD is a command"
  (str:starts-with-p "!" word))

(defun command-dispatch (status)
  "parses STATUS content for a command word, and runs any function it has in *commands* with it as the argument"
  (let* ((command (find-if #'commandp
			   (str:words
			    (tooter:plain-format-html (tooter:content status)))))
	 (cmd-func (gethash command *commands*)))
    
    (when cmd-func
      (funcall cmd-func status)
      
      ;; if we've hit here we return t, just so the calling function knows
      ;;  that we actually did something
      t)))

(defun add-command (cmd function)
  "adds a command into our hash

CMD should be a string
FUNCTION should be a function that accepts a single parameter (a tooter:status object)"
  (setf (gethash cmd *commands*) function))
