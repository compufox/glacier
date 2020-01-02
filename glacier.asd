;;;; glacier.asd

(asdf:defsystem #:glacier
  :description "lightweight mastodon bot framework"
  :author "ava fox"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :depends-on (#:dexador #:bordeaux-threads
	       #:websocket-driver #:simple-config
	       #:with-user-abort #:cl-json)
  :components ((:module "bots" :depends-on ("package")
		:serial t
		:components
		((:file "bot")))
	       
	       (:file "package")
	       (:file "util")
               (:file "glacier")))
