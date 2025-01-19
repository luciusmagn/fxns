;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/format
        :gerbil/gambit
        :std/srfi/9
        (only-in :std/srfi/1   every any)
        (only-in :std/srfi/133 vector-every vector-any))
(export #t)

(defsyntax fn
  (lambda (stx)
    (syntax-case stx (-> :)
      ((fn retfn func-name (params ... -> ret) body ...)
       (let loop ((params (syntax->list #'(params ...)))
                  (names '())
                  (checks '()))
         (if (null? params)
           (with-syntax ((func #'func-name)
                         ((param-names ...) (reverse names))
                         ((checks-code ...)
                          (map (lambda (name check)
                                 (with-syntax ((n name)
                                               (c check))
                                   #'(unless ((lambda (pred-or-list val)
                                                (if (list? pred-or-list)
                                                  (any (lambda (p) (p val)) pred-or-list)
                                                  (pred-or-list val)))
                                              c n)
                                       (error (format "Parameter check failed: [~a] doesn't satisfy predicate [~a]"
                                                      'n 'c)))))
                               (reverse names)
                               (reverse checks)))
                         (return-pred #'ret))
             #'(define (func param-names ...)
                 (let ((result
                        (call/cc
                          (lambda (raw-return)
                            (let ((retfn (lambda (result)
                                           (unless (return-pred result)
                                             (error (format "Return value check failed: ~a\n" 'ret)))
                                           (raw-return result))))
                              checks-code ...
                              body ...)))))
                   (unless (return-pred result)
                     (error (format "Return value type mismatch: ~a\n" 'ret)))
                   result)))
           (syntax-case (car params) (:)
             (var
              (identifier? #'var)
              (loop (cdr params)
                    (cons #'var names)
                    (cons #'(true) checks)))
             ((var : pred)
              (loop (cdr params)
                    (cons #'var names)
                    (cons #'pred checks))))))))))
(define (any? x) #t)
(define (true)   #t)

(define (list-of pred1 . preds)
  (let ((all-preds (cons pred1 preds)))
    (lambda (param)
      (if (list? param)
        (every (lambda (p) (any (lambda (pred?) (pred? p)) all-preds))
               param)
        #f))))


(define (vector-of pred1 . preds)
  (let ((all-preds (cons pred1 preds)))
    (lambda (param)
      (if (vector? param)
        (vector-every (lambda (p) (any (lambda (pred?) (pred? p)) all-preds))
                      param)
        #f))))

(define-record-type <option>
  (option tag data)
  option?
  (tag    option-tag)
  (val    option-val))

(fn :ret some ((x : any?) -> option?)
    (option 'some x))

(fn :ret none ( -> option?)
    (option 'none #f))

(fn :ret is-some? ((opt : option?) -> boolean?)
    (eqv? (option-tag opt) 'some))

(fn :ret is-none? ((opt : option?) -> boolean?)
    (eqv? (option-tag opt) 'none))

;; (option-of my-type?)
(fn :ret option-of ((pred : any?) -> procedure?)
    (lambda (opt)
      (if (option? opt)
        (cond
         ((is-some? opt) (pred (option-val opt)))
         ((is-none? opt) #t))
        #f)))

(define-record-type <result>
  (result tag data)
  result?
  (tag    result-tag)
  (val    result-val))

(fn :ret ok ((x : any?) -> result?)
    (result 'ok x))

(fn :ret err ((e : any?) -> result?)
    (result 'err #f))

;; (result-of string? my-error?)
(fn :ret result-of ((pred1 : any?) (pred2 : any?) -> procedure?)
    (lambda (res)
      (if (result? res)
        (or (and (eqv? (result-tag res) 'ok)  (pred1 (result-val res)))
            (and (eqv? (result-tag res) 'err) (pred2 (result-val res))))
        #f)))
