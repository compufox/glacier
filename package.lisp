;;;; package.lisp

(defpackage #:glacier
  (:use #:cl #:with-user-abort)
  (:import-from :simple-config
		:load-config
		:config))
