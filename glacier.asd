;;;; glacier.asd

(asdf:defsystem #:glacier
  :description "lightweight mastodon bot framework"
  :author "ava fox"
  :license  "BSD 3-Clause"
  :version "0.2"
  :depends-on (#:dexador #:bordeaux-threads
	       #:websocket-driver #:simple-config
	       #:with-user-abort #:cl-json
	       #:tooter #:cl-ppcre #:str)
  :serial t
  :components ((:file "package")
	       (:file "bot")
	       (:file "extensions")
	       (:file "util")
               (:file "glacier")))
