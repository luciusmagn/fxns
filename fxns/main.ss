;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/test
        ./lib)
(export main)

(include "../manifest.ss")

;; Basic function definition tests
(define basic-tests
  (test-suite "Basic function definitions"
    (test-case "Simple function with type checks"
      (fn :ret add1 ((x : number?) -> number?)
          (+ x 1))
      (check equal?  (add1 1) 2)
      (check-not-eq? (try
                      (add1 "not-number")
                      #t
                      (catch (e) #f))
                     #t))

    (test-case "Multiple argument function"
      (fn :ret sum3 ((x : number?) (y : number?) (z : number?) -> number?)
          (+ x y z))
      (check equal?  (sum3 1 2 3) 6)
      (check-not-eq? (try
                      (sum3 1 "2" 3)
                      #f ;; should error out
                      (catch (e) #f))
                     #t))

    (test-case "Early return"
      (fn :ret check-positive ((x : number?) -> boolean?)
          (when (< x 0)
            (:ret #f))
          #t)
      (check equal? (check-positive 1) #t)
      (check equal? (check-positive -1) #f))))

;; List and vector predicate tests
(define collection-tests
  (test-suite "Collection predicates"
    (test-case "list-of usage"
      (define num-list? (list-of number?))
      (check equal? (num-list? '(1 2 3)) #t)
      (check equal? (num-list? '(1 "2" 3)) #f)

      (fn :ret sum-list ((xs : (list-of number?)) -> number?)
          (apply + xs))
      (check equal? (sum-list '(1 2 3)) 6)
      (check-not-eq?
       (try
        (sum-list '(1 "2" 3))
        #t
        (catch (e) #f))
       #t))

    (test-case "vector-of usage"
      (define str-vec? (vector-of string?))
      (check equal? (str-vec? #("a" "b" "c")) #t)
      (check equal? (str-vec? #(1 2 3)) #f))))

;; Option type tests
(define option-tests
  (test-suite "Option type"
    (test-case "Basic option usage"
      (fn :ret find-even ((x : number?) -> option?)
          (if (even? x)
            (some x)
            (none)))
      (check equal? (option? (find-even 2)) #t)
      (check equal? (option? (find-even 3)) #t))

    (test-case "option-of predicate"
      (define num-opt? (option-of number?))
      (check equal? (num-opt? (some 42)) #t)
      (check equal? (num-opt? (some "str")) #f)
      (check equal? (num-opt? (none)) #t))))

;; Result type tests
(define result-tests
  (test-suite "Result type"
    (test-case "Basic result usage"
      (fn :ret safe-div ((x : number?) (y : number?) -> result?)
          (if (zero? y)
            (err "Division by zero")
            (ok (/ x y))))
      (check equal? (result? (safe-div 6 2)) #t)
      (check equal? (result? (safe-div 6 0)) #t))

    (test-case "result-of predicate"
      (def num-result? (result-of number? string?))
      (check equal? (num-result? (ok 42)) #t)
      (check equal? (num-result? (ok "str")) #f)
      (check equal? (num-result? (err "error msg")) #f)
      (check equal? (num-result? (err 42)) #f))))

;; Multiple predicate tests
(define multi-pred-tests
  (test-suite "Multiple predicates"
    (test-case "Function with predicate list"
      (fn :ret parse-num ((x : (list string? number?)) -> number?)
          (if (string? x)
            (string->number x)
            x))
      (check equal? (parse-num "42") 42)
      (check equal? (parse-num 42) 42)
      (check equal?
             (try
              (parse-num #t)
              #t
              (catch (e) #f))
             #f))))

(define all-tests
  (list
   basic-tests
   collection-tests
   option-tests
   result-tests
   multi-pred-tests))

(define (main . args)
  (for-each run-test-suite! all-tests))
