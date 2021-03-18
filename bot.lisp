(in-package #:glacier)

(defvar *bot* nil
  "global bot object")

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

(defmethod initialize-instance :after ((bot mastodon-bot) &rest initargs
                                       &key config-file instance token strip-html strip-username timeline
                                       &allow-other-keys)
  (declare (ignorable initargs))
  (when config-file
    (load-config config-file))
  (macrolet ((setf-unless (place value)
               `(unless ,place
                  (setf ,place ,value))))
    (setf-unless (config :strip-html) strip-html)
    (setf-unless (config :strip-bot-username) strip-username)
    (setf-unless (config :timeline) timeline)
    (setf-unless (config :mastodon-instance) instance)
    (setf-unless (config :mastodon-token) token))
  
  (let* ((client (make-instance 'bot-client
                                :access-token (config :mastodon-token)
                                :base (add-scheme (config :mastodon-instance))))
	 (account (tooter:verify-credentials client)))
    (setf (slot-value bot 'client) client
	  (slot-value bot 'account-id) (tooter:id account)
	  (slot-value bot 'account-username) (concatenate 'string "@" (tooter:username account)))))

(defun make-bot (&key config-file instance access-token (strip-html t) strip-username (timeline "user")
                   on-update on-delete on-notification)
  "makes a bot and returns it. 
INSTANCE, ACCESS-TOKEN, STRIP-HTML, STRIP-USERNAME, TIMELINE are all options that are typically in a config file
passing these values in allows the developer to skip specifying a config file and can pull values in from other places
e.g., command line arguments

CONFIG-FILE is a string or a path that denotes a glacier config file
INSTANCE is a mastodon instance domain name, with or without http scheme
ACCESS-TOKEN is an access token for a mastodon account on INSTANCE
STRIP-HTML if non-nil incoming posts will have their html stripped from them. defaults to T
STRIP-USERNAME if non-nil strips the bot's username from incoming posts. defaults to NIL
TIMELINE string denoting which timeline should be used for the streaming websocket. can be one of 'user', 'public', 'direct'. defaults to 'user'
ON-UPDATE a function that accepts a single mastodon status. gets ran for every new post that streams in from TIMELINE
ON-DELETE a function that accepts a single status id. gets ran for every deleted status that streams in from TIMELINE
ON-NOTIFICATION a function that accepts a single mastodon notification. gets ran for every notification that streams in from TIMELINE"
  (make-instance 'mastodon-bot :config-file config-file :instance instance :strip-html strip-html
                               :strip-username strip-username :timeline timeline :token access-token
                               :on-update on-update :on-delete on-delete
                               :on-notification on-notification))

