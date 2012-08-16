; -*- mode: scheme -*-
(define-library (scheme read)
 (export read)
 (import (ds base))
 (begin

 (define (read #(o (current-input-port)))
   (as o InputPort ReadDatum))

 );begin
);library