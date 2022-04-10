#lang racket

(define-struct column-info (name type)
  #:transparent)

(define-struct table (schema rows)
  #:transparent)


(define (check-type annotation)
  (cond
    [(equal? annotation 'string) (lambda (x) (string? x))]
    [(equal? annotation 'number) (lambda (x) (number? x))]
    [(equal? annotation 'boolean) (lambda (x) (boolean? x))]
    [(equal? annotation 'symbol) (lambda (x) (symbol? x))]))

(define (table-insert row tab)
  (let ([schema (table-schema tab)])
    (if (andmap (lambda (type col) ((check-type (column-info-type type)) col)) schema row)
        (cons row (table-rows tab))
        (raise "Invalid row" #f))))


;; TEST DATA - will be replaced with tests

(define cities
  (table (list (column-info 'city 'string)
               (column-info 'country 'string)
               (column-info 'area 'number)
               (column-info 'capital 'boolean))
         (list (list "Wrocław" "Poland" 293 #f)
               (list "Warsaw" "Poland" 517 #t)
               (list "Poznań" "Poland" 262 #f)
               (list "Berlin" "Germany" 892 #t)
               (list "Munich" "Germany" 310 #f)
               (list "Paris" "France" 105 #t)
               (list "Rennes" "France" 50 #f))))

(define countries
  (table (list (column-info 'country 'string) (column-info 'population 'number))
         (list (list "Poland" 38) (list "Germany" 83) (list "France" 67) (list "Spain" 47))))
