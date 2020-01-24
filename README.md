# ![glacier logo](https://i.imgur.com/tHNtPgc.png)
### _ava fox_

lightweight wrapper for making mastodon bots

## Installation

clone this repo into your local system definitions or `~/common-lisp` folder

## Usage

create a bot object, passing it (at minimum) a path to your config file

then pass that bot object to `(glacier:run-bot)`

if you pass nothing else but the bot object,
the framework will drop into a loop monitoring for updates
and running the proper handlers from your bot
(assuming you specified what handlers to run when creating your bot object)

you can pass code along with your bot to `(glacier:run-bot)` and it will
run that code along with monitoring for streaming updates

**NOTE**: if you pass code that doesn't block forever, the bot will exit without doing anything

```lisp
;; bad example
(glacier:run-bot my-bot
  (format t "my bot is running!")) ;; this doesn't block, so we print this and exit
```

see below for good examples

## Examples

the following runs a bot (using the instance and access token specified in the config)
that posts "trans rights are human rights" every 30 minutes

please see the example config for option names

```lisp
(glacier:run-bot (make-instance 'glacier:mastodon-bot :config-file "/path/to/bot.config")
  (glacier:after-every (30 :minutes)
    (glacier:post "trans rights are human rights" :visibility :public)))
```

the following runs a bot that responds to a mention with a cordial hello

```lisp
(defun maybe-respond (notification)
  "respond to a status from a mention NOTIFICATION"
  (when (glacier:mention-p notification)
    (glacier:reply (tooter:status notification) "hi! :3")))

(glacier:run-bot (make-instance 'glacier:mastodon-bot :config-file "/path/to/bot.config"
                                                      :on-notification #'maybe-respond))
```

the following runs a bot that will respond to posts with `!hello` in 
them with status personalized with their displayname

```lisp
(defun cordial-reply (status)
  (glacier:reply status (format nil "hi, ~a! :3"
                        (tooter:display-name (tooter:account status)))))

(glacier:add-command "hello" #'cordial-reply)

(glacier:run-bot (make-instance 'glacier:mastodon-bot :config-file "/path/to/bot.config"))
```

## Helper Functions

`after ((amount duration &key async) &body body))`

runs BODY after AMOUNT of DURATION time has passed

if ASYNC is non-nil runs asynchronously

```lisp
(after (3 :minutes) (print "hello"))

;; (after 3 minutes)
;;=> "hello"
```

---

`after-every ((amount duration &key async) &body body)`

same as AFTER except repeats after every duration

---

`post (text &key (visibility :unlisted) cw media sensitive)`

a thin wrapper around TOOTER:MAKE-STATUS 

`(post "hi~" :visibility :public)`


---

`reply (status text &key include-mentions media)`

replys to STATUS with TEXT

if include-mentions is non-nil then the reply will contain **all** mentions from the original status

NOTE: reply will **always** include an @ to the person it's replying to

---

`add-command (cmd function &key privileged)`

adds a command with CMD being the text to trigger the command and FUNCTION being the function that runs

FUNCTION needs to accept one parameter: a tooter:status object

if PRIVILEGED is non-nil, the bot needs to be following the account the mention comes from
for the command to be triggered

---

## License

BSD 3-Clause

