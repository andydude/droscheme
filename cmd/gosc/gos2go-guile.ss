(load "base-guile.ss")
(load "common-guile.ss")

(define emit-defines emit-defines-export)
(define emit-define-func emit-define-func-compile)
(define emit-import emit-import-blank)
(define emit-package emit-package-compile)

(main-guile)
