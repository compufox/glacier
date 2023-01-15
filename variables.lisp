(in-package :glacier)

(defvar *bot* nil
  "global bot object")

(defvar *commands* (make-hash-table :test #'equal)
  "hash table containing the bot's commands

KEY is the command as a string
VALUE is a function that accepts a tooter:status object as a parameter")

(defvar *command-prefix* "!"
  "character or string that prefixes a command")

(defvar *privileged-commands* (make-hash-table :test #'equal)
  "hash table containing commands that will only be ran if the mention
is from an account the bot follows

KEY is the command as a string
VALUE is a function that accepts a tooter:status object as a parameter")

(defvar *mappings-write-date* nil
  "")
(defvar *cw-mappings* nil
  "")
