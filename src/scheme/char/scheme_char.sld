(define-library (scheme char)
  (export
    char-ci<=?
    char-ci<?
    char-ci=?
    char-ci>=?
    char-ci>?
    char-downcase
    char-foldcase
    char-upcase
    digit-value
    string-ci=?
    string-ci<=?
    string-ci<?
    string-ci=?
    string-ci>=?
    string-ci>?
    string-downcase
    string-foldcase
    string-upcase)
  (import
   (ds base)))
