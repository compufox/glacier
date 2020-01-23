# glacier
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
  (glacier:after-every 30 :minutes
    (glacier:post "trans rights are human rights" :visibility :public)))
```

the following runs a bot that responds to a mention with a cordial hello

```lisp
(defun maybe-respond (notification)
  "respond to a status from a mention NOTIFICATION"
  (when (eq (tooter:kind notification) :mention)
    (glacier:reply (tooter:status notification) "hi! :3")))

(glacier:run-bot (make-instance 'glacier:mastodon-bot :config-file "/path/to/bot.config"
                                                      :on-notification #'maybe-respond))
```

## Helper Functions

`after (amount duration &body body))`

asynchronously runs BODY after AMOUNT of DURATION time has passed

```lisp
(after 3 :minutes (print "hello"))

;; (after 3 minutes)
;;=> "hello"
```

---

`after-every (amount duration &body body)`

same as AFTER except repeats after every duration

---

`post (text &key (visibility :unlisted) cw media sensitive)`

a thin wrapper around TOOTER:MAKE-STATUS 

`(post "hi~" :visibility :public)`


## Todo

- bots that react to commands

## License

BSD 3-Clause

