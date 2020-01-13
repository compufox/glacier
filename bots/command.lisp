(in-package :glacier)

(defclass command-bot (mastodon-bot)
  ((command-prefix :initarg :prefix
		   :initform "!"
		   :reader bot-command-prefix)
   (commands :initarg :commands
	     :accessor bot-commands)
   (command-description :initform (make-hash-table :test 'equal)
			:reader bot-command-descriptions)
   (command-functions :initform (make-hash-table :test 'equal)
		      :reader bot-command-functions)))

(defmethod generate-help ((bot command-bot))
  "generates a lambda that replies to a post with all the descriptions and functions")

(defmethod get-command ((bot command-bot) (command string))
  (gethash command (bot-command-functions bot)))

(defmethod add-command ((bot command-bot) (command string) function &optional description)
  (push command (bot-commands bot))
  (setf (gethash command (bot-command-functions bot)) function
	(gethash command (bot-command-descriptions bot)) (or description ""))
  (generate-help bot))

(defun make-command-bot (&rest args)
  (let ((bot (apply #'make-instance `(command-bot ,@args))))
    (add-command bot "help" #'help-reply "prints this help")))
