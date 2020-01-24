;;;; package.lisp

(defpackage #:glacier
  (:use #:cl #:with-user-abort)
  (:import-from :simple-config
		:load-config
		:config)
  (:export :run-bot
	   :mastodon-bot
	   :bot-on-update
	   :bot-on-delete
	   :bot-on-notification
           :after-every
	   :after
	   :post
	   :reply
	   :no-bot-p
	   :add-command
	   
	   :follow-request-p
           :poll-ended-p
	   :follow-p
           :boost-p
	   :fave-p
	   :mention-p))
