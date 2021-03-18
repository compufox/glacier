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
(glacier:run-bot (my-bot)
  (format t "my bot is running!")) ;; this doesn't block, so we print this and exit
```

see below for good examples

most objects that get passed around are [tooter objects](https://shinmera.github.io/tooter), so it probably wouldnt hurt to get acquainted with that library.

## Examples

the following runs a bot (using the instance and access token specified in the config)
that posts "trans rights are human rights" every 30 minutes

please see the example config for option names

```lisp
(glacier:run-bot ((make-bot :instance "mastodon.social" :access-token "n0tArealT0KEn"))
  (glacier:after-every (30 :minutes)
    (glacier:post "trans rights are human rights" :visibility :public)))
```

the following runs a bot that responds to a mention with a cordial hello

```lisp
(defun maybe-respond (notification)
  "respond to a status from a mention NOTIFICATION"
  (when (glacier:mention-p notification)
    (glacier:reply (tooter:status notification) "hi! :3")))

(glacier:run-bot ((make-bot :config-file "/path/to/bot.config" :on-notification #'maybe-respond)))
```

the following runs a bot that will respond to posts with `!hello` in 
them with status personalized with their displayname

```lisp
(defun cordial-reply (status)
  (glacier:reply status (format nil "hi, ~a! :3"
                        (tooter:display-name (tooter:account status)))))

(glacier:add-command "hello" #'cordial-reply)

(glacier:run-bot ((make-bot :config-file "/path/to/bot.config")))
```

## API

`make-bot (&key config-file instance access-token (strip-html t) strip-username (timeline "user") on-update on-delete on-notification)`

makes a bot and returns it. 

INSTANCE, ACCESS-TOKEN, STRIP-HTML, STRIP-USERNAME, TIMELINE are all options that are typically in a config file
passing these values in allows the developer to skip specifying a config file and can pull values in from other places
e.g., command line arguments

CONFIG-FILE is a string or a path that denotes a glacier config file

INSTANCE is a mastodon instance domain name, with or without http scheme

ACCESS-TOKEN is an access token for a mastodon account on INSTANCE

STRIP-HTML if non-nil incoming posts will have their html stripped from them. defaults to T

STRIP-USERNAME if non-nil strips the bot's username from incoming posts. defaults to NIL

TIMELINE string denoting which timeline should be used for the streaming websocket. can be one of 'user', 'public', 'direct'. defaults to 'user'

ON-UPDATE a function that accepts a single mastodon status. gets ran for every new post that streams in from TIMELINE

ON-DELETE a function that accepts a single status id. gets ran for every deleted status that streams in from TIMELINE

ON-NOTIFICATION a function that accepts a single mastodon notification. gets ran for every notification that streams in from TIMELINE

---

`run-bot ((bot &key delete-command (with-websocket t)) &body body)`

runs BOT, setting up websocket handlers and starting the streaming connection before executing BODY

if DELETE-COMMAND is non-nil, automatically adds a delete command
if WITH-WEBSOCKET is non-nil (default), automatically starts up a websocket listener for realtime updates

NOTE: DELETE-COMMAND is ignored if WITH-WEBSOCKET is nil

if BODY is not provided drops into a loop where we sleep until the user quits us, or our connection closes. this functionality does not happen if WITH-WEBSOCKET is nil.

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

`mention-p (notification)`

returns T if NOTIFICATION is a mention

---

`fave-p (notification)`

returns T if NOTIFICATION is a favourite

---

`boost-p (notification)`

returns T if NOTIFICATION is a boost

---

`poll-ended-p (notification)`

returns T if NOTIFICATION is from a poll ending

---

`follow-request-p (notification)`

returns T if NOTIFICATION is a follow-request

---

`follow-p (notification)`

returns T if NOTIFICATION is a follow

---

`bot-post-p (status)`

returns T if STATUS was posted by the bot

---

`no-bot-p (account-or-mention)`

returns T if ACCOUNT (or account from MENTION) has #NoBot in their profile's bio

---

`delete-parent (status)`

deletes the parent to STATUS if it was made by the bot

---

`after ((amount duration &key async) &body body)`

runs BODY after AMOUNT of DURATION time has passed

if ASYNC is non-nil runs asynchronously

```lisp
(after (3 :minutes) (print "hello"))

;; (after 3 minutes)
;;=> "hello"
```

---

`after-every ((amount duration &key async run-immediately) &body body)`

same as AFTER except repeats after every duration

if RUN-IMMEDIATELY is non-nil, executes BODY once before waiting for the next iteration

---

`on ((day &key at async) &body body`

runs BODY on DAY, optionally AT a time

DAY is a keyword with the day of the week (e.g., :sunday, :monday, etc)

AT is a string denoting a time (e.g., "13:20", "4:20PM", "23:00")

if ASYNC is non-nil code is executed asynchronously

if AT is nil, code is ran at midnight on DAY

---

`add-command (cmd function &key privileged add-prefix)`

adds a command with CMD being the text to trigger the command and FUNCTION being the function that runs

FUNCTION needs to accept one parameter: a tooter:status object

if PRIVILEGED is non-nil, the bot needs to be following the account the mention comes from
for the command to be triggered

if ADD-PREFIX is non-nil, adds \*COMMAND-PREFIX\* to the front of CMD

---

`*command-prefix*`

the string that prefixes commands. 

defaults to "!"

---

`terminate-connection`

terminates the websocket connection that feeds the bot streaming updates

effectively terminates the bot

---

## License

BSD 3-Clause

