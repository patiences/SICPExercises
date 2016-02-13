**2.27**
```
;; (listof X) -> (listof X)
;; produce the list reversed, as well as all of the sublists reversed 

(check-expect (deep-reverse (list (list 1 2) (list 3 4))) 
              (list (list 4 3) (list 2 1)))
(check-expect (reverse (list 1 2 3 4 5))
              (list 5 4 3 2 1))


(define (deep-reverse lox) 
  (cond [(empty? lox) empty]
        [else 
         (append (deep-reverse (cdr lox))
                 (list (fn-for-x (car lox))))]))

(define (fn-for-x x)
  (cond [(list? x) (deep-reverse x)]
        [else 
         (identity x)]))
```

**2.28**

```
(check-expect (fringe (list (list 1 2) (list 3 4)))
              (list 1 2 3 4))

(define (fringe lox)
  (cond [(empty? lox) empty]
        [(list? (car lox)) 
         (append (fringe (car lox))
                 (fringe (cdr lox)))]
        [else
         (cons (car lox)
               (fringe (cdr lox)))]))
```
