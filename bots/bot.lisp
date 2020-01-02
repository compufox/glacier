(in-package #:glacier)

(defclass bot ()
  ((on-update :initarg :on-update
	      :accessor bot-on-update)
   (on-delete :initarg :on-delete
	      :accessor bot-on-delete)
   (on-notification :initarg :on-notification
		    :accessor bot-on-notification)
   (account-id :initarg :account-id
	       :accessor bot-account-id))
  (:documentation "bot superclass"))
