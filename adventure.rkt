#lang racket

(provide define-verbs
         define-item
         define-place

         show-current-place
         show-inventory

         item-state
         have-item?
         take-item!
         drop-item!
         set-item-state!

         (except-out (all-from-out racket) #%module-begin)
         (rename-out [module-begin #%module-begin]))

;; ============================================================
;; Overall module:

(define-syntax module-begin
  (syntax-rules (define-verbs)
    [(_ (define-verbs all-verbs cmd ...)
        decl ...
        starting-place)
     (#%module-begin
      (define-verbs all-verbs cmd ...)
      decl ...
      (start-game starting-place all-verbs))]))

;; Elements of the world:
(struct verb (aliases            ; list of symbols
              desc               ; string
              thing?))           ; boolean
(struct item (name               ; symbol
              [state #:mutable]  ; state of item
              actions))          ; list of verb -> function conses
(struct place (desc              ; string
               [items #:mutable] ; list of items
               actions))         ; list of verb -> function conses

;; Macros for constructing and registering elements:

(define-syntax-rule (define-verbs all-id
                      [(id aliases ...) spec ...] ...)
  (begin
    (define-one-verb (id aliases ...) spec ...) ...
    (define all-id (list id ...))))

(define-syntax define-one-verb
  (syntax-rules (thing)
    [(_ (id ...) desc)
     (begin
       (define id (verb (list 'id ...) desc #f))
       ...)]
    [(_ (id ...) thing desc)
     (begin
       (define id (verb (list 'id ...) desc #t))
       ...)]))

(define-syntax-rule (define-item id
                      start-state
                      [vrb expr exprs ...] ...)
  (define id
    (item
      'id
      start-state
      (list (cons vrb (λ () expr exprs ...)) ...))))

(define-syntax-rule (define-place id
                      desc
                      (thing ...)
                      ([vrb expr exprs ...] ...))
  (define id
    (place desc
           (list thing ...)
           (list (cons vrb (λ () expr exprs ...)) ...))))

;; ============================================================
;; Game state

;; Initialized on startup:
(define all-verbs '())          ; list of verbs

;; Things carried by the player:
(define inventory '()) ; list of items

;; Current location:
(define current-place #f) ; place (or #f until started)

;; Fuctions to be used by verb responses:
(define (have-item? item)
  (memq item inventory))

(define (take-item! item)
  (set-place-items!
    current-place
    (remq item (place-items current-place)))
  (set! inventory (cons item inventory)))

(define (drop-item! item)
  (set-place-items!
    current-place
    (cons item (place-items current-place)))
  (set! inventory (remq item inventory)))

;; Game execution

;; Show the player the current place, then get a command:
(define (handle-place)
  (show-current-place)
  (handle-verb))

;; Show the current place:
(define (show-current-place)
  (printf "~a~%" (place-desc current-place))
  (for-each (lambda (item)
              (printf "There is a ~a here.~%" (item-name item)))
            (place-items current-place)))

;; Get and handle a command:
(define (handle-verb)
  (display "> ")
  (flush-output)
  (let* ([line (read-line)]
         [input (if (eof-object? line)
                    '(quit)
                    (let ([port (open-input-string line)])
                      (for/list ([v (in-port read port)]) v)))])

    (define (handle-result verb-result)
      (cond
        [(place? verb-result)
         (set! current-place verb-result)
         (show-current-place)]
        [(string? verb-result)
         (displayln verb-result)]
        [else #f]))

    (match input
      [(list (or 'quit 'exit))
       (displayln "Bye!")
       (exit)]
      [(list (or 'inventory 'items))
       (show-inventory)]
      [(list 'look)
       (show-current-place)]
      [(list verb thing)
       (handle-result
         ((handle-thing-verb verb thing)))]
      [(list verb)
       (handle-result
         ((handle-simple-verb verb)))]
      [_ (displayln "I don't know what that means.")])
    (handle-verb)))

;; Handle a verb which does not take an item:
(define (handle-simple-verb vrb)
  (or
   (find-verb vrb (place-actions current-place))
   (using-verb
    vrb all-verbs
    (lambda (verb)
      (lambda ()
        (if (verb-thing? verb)
          (format "~a what?" (string-titlecase (verb-desc verb)))
          (format "Can't ~a here." (verb-desc verb))))))
   (lambda ()
     (format "I don't know how to ~a." vrb))))

;; Handle a verb which does take an item:
(define (handle-thing-verb vrb obj)
  (or (using-verb
        vrb
        all-verbs
        (lambda (verb)
          (and
            (verb-thing? verb)
            (let ([thing (memf (λ (item)
                                 (and (eq? (item-name item) obj)
                                      item))
                               (append (place-items current-place)
                                       inventory))])
            (if thing
              (or (find-verb vrb (item-actions (car thing)))
                  (λ ()
                    (format "Don't know how to ~a ~a."
                            (verb-desc verb) obj)))

              (λ ()
                (format "There's no ~a here to ~a." obj
                        (verb-desc verb))))))))
      (λ ()
        (format "I don't know how to ~a ~a." vrb obj))))

;; Show what the player is carrying:
(define (show-inventory)
  (display "You have")
  (if (null? inventory)
      (display " no items.")
      (for-each (lambda (item)
                  (printf "~%  a ~a" (item-name item)))
                inventory))
  (newline))

;; Look for a command match in a list of verb--response pairs,
;; and returns the response thunk if a match is found:
(define (find-verb cmd actions)
  (ormap (lambda (a)
           (and (memq cmd (verb-aliases (car a)))
                (cdr a)))
         actions))

;; Looks for a command in a list of verbs, and
;; applies `suucess-k' to the verb if one is found:
(define (using-verb cmd verbs success-k)
  (ormap (lambda (vrb)
           (and (memq cmd (verb-aliases vrb))
                (success-k vrb)))
         verbs))

(define (start-game in-place
                    in-all-verbs)
  (set! current-place in-place)
  (set! all-verbs in-all-verbs)
  (handle-place))
