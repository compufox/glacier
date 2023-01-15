;;;; glacier.asd

(asdf:defsystem #:glacier
  :description "lightweight mastodon bot framework"
  :author "ava fox"
  :license  "BSD 3-Clause"
  :version "0.4"
  :depends-on (#:dexador #:bordeaux-threads #:str
	       #:websocket-driver #:simple-config #:cl-json
	       #:tooter #:cl-ppcre #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "variables")
	       (:file "util")
               (:file "mappings")
	       (:file "bot")
	       (:file "extensions")
               (:file "glacier")))
