;;; extensions.lisp
;; extensions and helper functions for
;;  tooter objects

(in-package #:glacier)

(defvar *no-bot-regex* "(?i)#?NoBot"
  "regex to check for the NoBot tag")

(defmethod no-bot-p ((id integer))
  "checks an account's bio and profile fields to see if they contain a NoBot tag"
  (no-bot-p (tooter:find-account (bot-client *bot*) id)))

(defmethod no-bot-p ((account tooter:account))
  "checks an account's bio and profile fields to see if they contain a NoBot tag"
  (or (cl-ppcre:scan *no-bot-regex* (tooter:note account))
      (some #'identity (loop for (f . v) in (tooter:fields account)
			     collect (or (cl-ppcre:scan *no-bot-regex* f)
					 (cl-ppcre:scan *no-bot-regex* v))))))

(defmethod reply ((status tooter:status) text &key include-mentions)
  "replies to a STATUS with TEXT. copies the visibility and content warning as the post it's replying to

if INCLUDE-MENTIONS is non-nil, include mentions besides the primary account being replied to"
  (let* ((client (bot-client *bot*))
	 (reply-account (tooter:account status))
	 (reply-mentions (loop for mention in (tooter:mentions status)
			    unless (string= (tooter::account-name mention)
					    (tooter::account-name reply-account))
			    collect (concatenate 'string "@" (tooter::account-name mention)))))
    (tooter:make-status client (str:join " "
					 `(,(concatenate 'string "@" (tooter::account-name reply-account))
					   ,@(when include-mentions reply-mentions)
					   ,text))
			:visibility (tooter:visibility status)
			:spoiler-text (tooter:spoiler-text status)
			:in-reply-to (tooter:id status))))

(defun post (text &key (visibility :unlisted) cw sensitive media)
  "a thin wrapper around tooter:make-status

see documentation for that function"
  (tooter:make-status (bot-client *bot*)
		      text
		      :visibility visibility
		      :spoiler-text cw
		      :media media
		      :sensitive sensitive))

		  
