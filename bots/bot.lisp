(in-package #:glacier)

(defclass bot-client (tooter:client)
  ((name :initform "GlacierBot")
   (website :initform "https://github.com/compufox/glacier")))

(defclass mastodon-bot ()
  ((on-update :initarg :on-update
	      :accessor bot-on-update)
   (on-delete :initarg :on-delete
	      :accessor bot-on-delete)
   (on-notification :initarg :on-notification
		    :accessor bot-on-notification)
   (account-id :initarg :account-id
	       :accessor bot-account-id)
   (client :reader bot-client))
  (:documentation "bot superclass"))

(defmethod initialize-instance :after ((instance mastodon-bot) &rest initargs
				       &key config-file &allow-other-keys)
  (load-config config-file)
  (setf (slot-value instance 'client)
	(make-instance 'bot-client
		       :access-token (config :mastodon-token)
		       :base (config :mastodon-instance))))
       
