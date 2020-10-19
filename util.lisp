(in-package #:glacier)

(declaim (inline fave-p boost-p mention-p follow-p poll-ended-p
		 follow-request-p bot-post-p agetf))

(defun parse-time (amount duration)
  "parses AMOUNT of DURATION into seconds"
  (* amount (cond
	     ((or (eq duration :seconds)
		  (eq duration :second))
	      1)
	     ((or (eq duration :minutes)
		  (eq duration :minute))
	      60)
	     ((or (eq duration :hours)
		  (eq duration :hour))
	      3600)
	     ((or (eq duration :days)
		  (eq duration :day))
	      86400)
	     ((or (eq duration :weeks)
		  (eq duration :week))
	      604800)
	     (t (error "unknown duration")))))

(defmacro after ((amount duration &key async) &body body)
  "runs BODY after AMOUNT of DURATION

if ASYNC is non-nil, runs asynchronously"
  (let ((code `((sleep (parse-time ,amount ,duration))
		,@body)))
    (if async
	`(bt:make-thread
	  (lambda () ,@code))
	`(progn ,@code))))

(defmacro after-every ((amount duration &key async run-immediately) &body body)
  "runs BODY after every AMOUNT of DURATION

if ASYNC is non-nil, runs asynchronously
if RUN-IMMEDIATELY is non-nil, runs BODY once before waiting for next invocation"
  (let ((code `(loop ,@(when run-immediately `(initially ,@body))
		     do (sleep (parse-time ,amount ,duration))
		     ,@body)))
    (if async
	`(bt:make-thread
	  (lambda () ,code))
	code)))

(defmacro on ((day &key at async) &body body)
  "runs BODY on DAY, optionally AT a time

DAY is a keyword with the day of the week (e.g., :sunday, :monday, etc)
AT is a string denoting a time (e.g., '13:20', '4:20PM', '23:00')

if ASYNC is non-nil code is executed asynchronously
if AT is nil, code is ran at midnight on DAY"
  (let ((code `(loop with wanted-dow = (get-dow-for ,day)
                     with executed = nil

                     do (loop for dow = (nth 6 (multiple-value-list (get-decoded-time)))
                              unless (and (= dow wanted-dow)
                                          (not executed))
                                do (sleep (1+ (seconds-until-midnight)))
                                   (setf executed nil)
                                   
                              until (= dow wanted-dow))
                        
                     ,(when at `(sleep (seconds-until-timestring ,at)))
                     (setf executed t)
                     ,@body)))
    (if async
        `(bt:make-thread
          (lambda () ,code))
        code)))

(defun get-dow-for (day)
  "returns the day of the week for DAY"
  (case day
    (:sunday 6)
    (:monday 0)
    (:tuesday 1)
    (:wednesday 2)
    (:thursday 3)
    (:friday 4)
    (:saturday 5)))

(defun seconds-until-midnight ()
  "returns how many seconds until it's midnight"
  (seconds-until 23 59 60))

(defun seconds-until (hours minutes &optional (seconds 0))
  "determines how many seconds until HOURS MINUTES and SECONDS
if the time has already passed it returns 0 instead of a negative time"
  (let* ((decoded (multiple-value-list (get-decoded-time)))
         (hour (nth 2 decoded))
         (minute (nth 1 decoded))
         (second (nth 0 decoded))
         (results (+ (parse-time (- hours hour) :hours)
                     (parse-time (- minutes minute) :minutes)
                     (- seconds second))))
    (if (< results 0)
        0
        results)))

(defun seconds-until-timestring (time-string)
  "parses TIME-STRING and returns how long until the time specified
TIME-STRING is of the form '12:04', '18:30', '1:10PM', etc"
  (let* ((split-time (str:split ":" time-string))
         (hours (parse-integer (first split-time)))
         (minutes (parse-integer (second split-time) :junk-allowed t)))
    (when (and (str:ends-with-p "PM" time-string)
               (< hours 12))
      (incf hours 12))
    (+ (seconds-until hours minutes))))

(defun agetf (place indicator &optional default)
  "getf but for alists"
  (or (cdr (assoc indicator place :test #'equal))
      default))

(defun get-mastodon-streaming-url ()
  "gets the websocket url for the mastodon instance"
  (gethash "streaming_api" (tooter:urls (tooter:instance (bot-client *bot*)))))

(defun print-open ()
  "prints a message when the websocket is connected"
  (print "connected"))

(defun print-close (&key code reason)
  "prints a message when the websocket is closed"
  (when (and code reason)
    (format t "disconnected because ~A (code=~A)~%" reason code)))

(defun add-scheme (domain)
  "adds https scheme to DOMAIN if it isnt already there"
  (if (search "https://" domain)
      domain
      (concatenate 'string
		   "https://"
		   domain)))

(defun commandp (word)
  "checks if WORD is a command"
  (or (member word (hash-table-keys *commands*) :test #'equal)
      (member word (hash-table-keys *privileged-commands*) :test #'equal)))

(defun add-command (cmd function &key privileged (add-prefix t))
  "adds a command into our hash

CMD should be a string
FUNCTION should be a function that accepts a single parameter (a tooter:status object)

if PRIVILEGED is non-nil, command will only be triggered if mention is sent by an account the bot is following
if ADD-PREFIX is non-nil, adds *COMMAND-PREFIX* to the front of CMD (defaults to t)"
  (setf (gethash (if add-prefix
		     (concatenate 'string *command-prefix* cmd)
		     cmd)
		 (if privileged
		     *privileged-commands*
		     *commands*))
	function))

(defun privileged-reply-p (status)
  "returns T if STATUS is from an account that the bot follows"
  (tooter:following (car
		     (tooter:relationships
		      (bot-client *bot*)
		      (list (tooter:id (tooter:account status)))))))

(defun fave-p (notification)
  "checks if NOTIFICATION is a favourite"
  (eq (tooter:kind notification) :favourite))

(defun mention-p (notification)
  "checks if NOTIFICATION is a mention"
  (eq (tooter:kind notification) :mention))

(defun boost-p (notification)
  "checks if NOTIFICATION is a boost"
  (eq (tooter:kind notification) :reblog))

(defun poll-ended-p (notification)
  "checks if NOTIFICATION is a poll"
  (eq (tooter:kind notification) :poll))

(defun follow-request-p (notification)
  "checks if NOTIFICATION is a follow request"
  (eq (tooter:kind notification) :follow-request))

(defun follow-p (notification)
  "checks if NOTIFICATION is a follow"
  (eq (tooter:kind notification) :follow))

(defun bot-post-p (status)
  "checks if STATUS was posted by the bot"
  (equal (bot-account-id *bot*) (tooter:id (tooter:account status))))

(defun delete-parent (status)
  "deletes the parent post of STATUS if it was posted by the bot"
  (let ((parent (tooter:find-status (bot-client *bot*) (tooter:in-reply-to-id status))))
    (when (bot-post-p parent)
      (tooter:delete-status (bot-client *bot*) parent))))
