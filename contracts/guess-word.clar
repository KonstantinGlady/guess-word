(define-constant THIS_CONTRACT (as-contract tx-sender))

(define-constant ERR_MIN_BET_AMOUNT u100)
(define-constant ERR_GAME_NOT_FOUND u101)
(define-constant ERR_GAME_CANNOT_BE_JOINED u102)
(define-constant ERR_INVALID_WORD u103)

(define-data-var latest-game-id uint u0)

(define-map games uint {
    game-owner: principal,
    player: (optional principal),
    bet-amount: uint,
    target-word: string-ascii 5,
    maximun-number-allowed-guesses: uint,
    attempt-number: uint,
    is-game-over: bool
})


(define-public (create-game (bet-amount uint) (target-word string-ascii 5))
  (let (
    (game-id (var-get latest-game-id))

    (game-data {
        game-owner: contract-caller,
        player: none,
        bet-amount: bet-amount,
        target-word: target-word,
        maximum-number-allowed-guess: 3,
        attempt-number: 0,
        is-game-over: false
      })
    )

    (asserts! (> bet-amount u0) (err ERR_MIN_BET_AMOUNT))
    (try! (stx-transfer? bet-amount contract-caller THIS_CONTRACT))

    (maps-set games game-id game-data)
    (var-set latest-game-id (+ game-id u1))
    (print {action: "create-game", data: game-data})

    (ok game-id)
))

(define-public (join-game (game-id uint) (guess-word string-ascii 5))
    (let (
            (original-game-data (unwrap! (map-get? games game-id) (err ERR_GAME_NOT_FOUND)))
            (game-data (merge original-game-data {
                player: (some contract-caller)
            } ))
    )

    (asserts! (is-none (get player original-game-data)) (err ERR_GAME_CANNOT_BE_JOINED))
   
    (try! (stx-transfer? (get bet-amount original-game-data) contract-caller THIS_CONTRACT))

    (map-set games game-id game-data)

    (print {action: "join-game", data: game-data})
    (ok game-id)
))

(define-private (is-correct (target-word string-ascii 5) (guess-word string-ascii 5))
    (is-eq target-word guess-word)
)

(define-public (attempt (game-id uint) (guess-word string-ascii 5))
    (let (
        (original-game-data (unwrap! (map-get? games game-id) (err ERR_GAME_NOT_FOUND)))
        (target-word (get target-word original-game-data))
        (player (get player original-game-data))
        (is-game-won (is-correct (target-word) (guess-word)))

        (game-data (merge original-game-data {
            attempt-number: + attempt-number u1,
            is-game-over: is-game-won
        }))



    )
        if(is-game-won (try! (as-contract (stx-transfer? (* u2 ( get bet-amount game-data)) tx-sender player))) false)
        (map-set games game-id game-data)

        (print {action: "attempt", data: game-data})
        (ok game-id)
    )
)

(define-read-only (get-game (game-id uint))
    (map-get? games game-id)
)

(define-read-only (get-latest-game-id)
    (var-get latest-game-id)
    (ok)
)
