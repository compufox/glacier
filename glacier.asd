;;;; glacier.asd

(asdf:defsystem #:glacier
  :description "lightweight mastodon bot framework"
  :author "ava fox"
  :license  "BSD 3-Clause"
  :version "0.3"
  :depends-on (#:bordeaux-threads #:str #:cl-json
	       #:websocket-driver-client #:simple-config 
	       #:tooter #:cl-ppcre #:alexandria #:drakma)
  :serial t
  :components ((:file "package")
	       (:file "bot")
	       (:file "extensions")
	       (:file "util")
               (:file "glacier")))
