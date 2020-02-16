;;;; package.lisp

(defpackage #:glacier
  (:use #:cl)
  (:import-from :simple-config
		:load-config
		:config)
  (:import-from :alexandria
		:hash-table-keys
		:flatten)
  (:export :run-bot
	   :mastodon-bot
	   :bot-on-update
	   :bot-on-delete
	   :bot-on-notification
	   :bot-username
           :after-every
	   :after
	   :post
	   :reply
	   :add-command
           :agetf
           :bot-client
	   :terminate-connection
	   :*command-prefix*
	   :no-bot-p
	   :follow-request-p
           :poll-ended-p
	   :follow-p
           :boost-p
	   :fave-p
	   :mention-p
	   :delete-parent
	   :bot-post-p))
