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
                                   #'(unless c
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
                                             (error "Return value check failed"))
                                           (raw-return result))))
                              checks-code ...
                              body ...)))))
                   (unless (return-pred result)
                     (error "Return value check failed"))
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
                    (cons #'(pred var) checks))))))))))


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
        (vector-every (lambda (p) (vector-any (lambda (pred?) (pred? p)) all-preds))
                      param)
        #f))))

(define-record-type <option>
  (option tag data)
  option?
  (tag    option-tag)
  (val    option-val))

(fn :ret some (x -> option?)
    (option 'some x))

(fn :ret none ( -> option?)
    (option 'none #f))

(define-record-type <result>
  (result tag data)
  result?
  (tag    result-tag)
  (val    result-val))

(fn :ret ok (x -> result?)
    (option 'ok x))

(fn :ret err (e -> result?)
    (option 'err #f))


;; Usage example
;;(fn :ret some-func ((x : number?)
;;                    (y : number?)
;;                    (z : number?)
;;                    -> boolean?)
;;    (when (zero? x)
;;      (:ret #t))

;;    #f)
