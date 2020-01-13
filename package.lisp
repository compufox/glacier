;;;; package.lisp

(defpackage #:glacier
  (:use #:cl #:with-user-abort)
  (:import-from :simple-config
		:load-config
		:config)
  (:import-from :
  (:export :run-bot
	   :mastodon-bot
	   :bot-on-update
	   :bot-on-delete
	   :bot-on-notification))
