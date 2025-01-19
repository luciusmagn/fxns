;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        ./lib)
(export main)

;; build manifest; generated during the build
;; defines version-manifest which you can use for exact versioning
(include "../manifest.ss")

(define (main . args)
  (displayln "this is a library"))
