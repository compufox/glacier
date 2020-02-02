(in-package #:glacier)

(defvar *commands* (make-hash-table :test #'equal)
  "hash table containing the bot's commands

KEY is the command as a string
VALUE is a function that accepts a tooter:status object as a parameter")

(defvar *command-prefix* "!"
  "character or string that prefixes a command")

(defvar *privileged-commands* (make-hash-table :test #'equal)
  "hash table containing commands that will only be ran if the mention
is from an account the bot follows

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
   (account-username :reader bot-username)
   (client :reader bot-client))
  (:documentation "bot superclass"))

(defmethod initialize-instance :after ((instance mastodon-bot) &rest initargs
				       &key config-file &allow-other-keys)
  (load-config config-file)
  (let* ((client (make-instance 'bot-client
				:access-token (config :mastodon-token)
				:base (add-scheme (config :mastodon-instance))))
	 (account (tooter:verify-credentials client)))
    (setf (slot-value instance 'client) client
	  (slot-value instance 'account-id) (tooter:id account)
	  (slot-value instance 'account-username) (concatenate 'string "@" (tooter:username account)))))
