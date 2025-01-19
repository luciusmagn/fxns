#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("fxns/lib"
    (exe: "fxns/main" bin: "fxns")))
