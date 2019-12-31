;;;; glacier.asd

(asdf:defsystem #:glacier
  :description "lightweight mastodon bot framework"
  :author "ava fox"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:dexador #:bordeaux-threads
	       #:websocket-driver #:simple-config
	       #:with-user-abort)
  :components ((:file "package")
               (:file "glacier")))
