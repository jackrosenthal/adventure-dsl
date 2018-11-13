#lang s-exp "adventure.rkt"

(define-verbs all-verbs
  [(north n) "go north"]
  [(south s) "go south"]
  [(east e) "go east"]
  [(west w) "go west"]
  [(up) "go up"]
  [(down) "go down"]
  [(in enter) "enter"]
  [(out leave) "leave"]
  [(get grab take) thing "take"]
  [(put drop) thing "drop"]
  [(open unlock) thing "open"]
  [(close lock) thing "close"]
  [(knock) thing "knock"])

;; Items

(define-item cactus #f
  [get "Ouch!"])

(define-item door 'closed
  [open (if (have-item? key)
          (begin
            (set-item-state! door 'open)
            "You use the key to unlock and open the door.")
          "The door is locked.")]
  [close (begin
           (set-item-state! door 'closed)
           "The door is now closed.")]
  [knock "No one is home."])

(define-item key #f
  [get (if (have-item? key)
         "You already have the key."
         (begin
           (take-item! key)
           "You now have the key."))]
  [put (if (have-item? key)
         (begin
           (drop-item! key)
           "You have dropped the key.")
         "You don't have the key.")])

(define-item trophy #f
  [get (take-item! trophy)
       "You win!"])

;; Places

(define-place meadow
  "You're standing in a meadow. There is a house to the north."
  ()
  ([north house-front]
   [south desert]))

(define-place house-front
  "You are standing in front of a house."
  (door)
  ([in (if (eq? (item-state door) 'open)
         room
         "The door is not open.")]
   [south meadow]))

(define-place desert
  "You're in a desert. There is nothing for miles around."
  (cactus key)
  ([north meadow]
   [south desert]
   [east desert]
   [west desert]))

(define-place room
  "You're in the house."
  (trophy)
  ([out house-front]))

;; Starting place ----------------------------------
;; The module must end with the starting place name:
meadow
