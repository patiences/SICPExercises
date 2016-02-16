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
**2.30**

```
(check-expect (square-tree 
               (list (list 1 2) 3 (list 4 5 6) (list (list 7) 8)))
              (list (list 1 4) 9 (list 16 25 36) (list (list 49) 64)))

#;
(define (square-tree t)
  (cond [(empty? t) empty]
        [else
         (cons (fn-for-node (car t))
               (square-tree (cdr t)))]))

(define (fn-for-node n)
  (cond [(list? n) (square-tree n)]
        [else
         (square1 n)]))

(define (square1 x)
  (* x x))

#;
(define (square-tree t)
  (map (λ (n) 
         (cond 
           [(empty? n) empty]
           [(not (list? n))
            (square n)]
           [else
            (square-tree n)]))
       t)) 
```

**2.31**

```
(define (square-tree tree) (tree-map square1 tree))

(define (tree-map f tree)
  (cond [(empty? tree) empty]
        [(list? (car tree)) 
         (cons (tree-map f (car tree))
               (tree-map f (cdr tree)))]
        [else
         (cons (f (car tree)) 
               (tree-map f (cdr tree)))]))
```
**2.33**

```
(define (map p sequence)
  (foldr (lambda (x y) (cons (p x) y)) empty sequence))
  
(define (append seq1 seq2)
  (foldr cons seq2 seq1))
  
(define (length sequence)
  (foldr (λ (x y) (+ 1 y)) 0 sequence))
```

**2.35**

```
(define (count-leaves tree)
  (foldr + 0 (map (λ (x)
                    (cond [(empty? x) empty]
                          [(list? x)
                           (count-leaves x)]
                          [else 1])) tree)))

(count-leaves (list (list 1 2 3) 4 (list 5) (list (list 6 7) 8)))
```

**2.36**

```
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      empty 
      (cons (foldr op init (map (λ (seq) (car seq)) seqs))
            (accumulate-n op init (map (λ (seq) (cdr seq)) seqs)))))
```
**2.59**

```
(define (union-set set1 set2)
    (cond [(null? set1) set2]
          [(null? set2) set1]
          [else 
           (if (member? (car set1) set2)
               (union-set (cdr set1) set2)
               (cons (car set1) (union-set (cdr set1) set2)))]))
```

**2.62**

```
(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [(= (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) (cdr set2)))]
        [(< (car set1) (car set2)) 
         (cons (car set1) (union-set (cdr set1) set2))]
        [(> (car set1) (car set2))
         (cons (car set2) (union-set set1 (cdr set2)))]))
```

**2.68**

```
(define-struct tree (node0 node1))
(define-struct leaf (symb wght))


(define HT1 (make-tree (make-leaf 'A 4)
                       (make-tree (make-leaf 'B 2)
                                  (make-tree (make-leaf 'D 1)
                                             (make-leaf 'C 1)))))
                                             
;; Symbol Tree -> (listof Natural) 

(check-expect (encode-symbol 'A HT1) (list 0))
(check-expect (encode-symbol 'D HT1) (list 1 1 0))

(define (encode-symbol sym tree)
  (cond [(leaf? tree) 
         (if (equal? sym (leaf-symb tree))
             empty
             false)]
        [else
         (if (not (false? (encode-symbol sym (tree-node0 tree))))
             (append (list 0) (encode-symbol sym (tree-node0 tree)))
             (if (not (false? (encode-symbol sym (tree-node1 tree))))
                 (append (list 1) (encode-symbol sym (tree-node1 tree)))
                 false))]))
```

**2.75**

```
(define (make-from-mag-ang r a)
  (local [(define (dispatch op)
            (cond [(equal? op 'real-part) (* r (cos a))]
                  [(equal? op 'imag-part) (* r (sin a))]
                  [(equal? op 'magnitude) r]
                  [(equal? op 'angle) a]
                  [else
                   (error "Unknown op -- MAKE-FROM-REAL-IMAG" op)]))]
    dispatch))
```

**2.87**

```
(define (=zero-poly? poly)
  (local [(define (zero-terms? termlist)
            (or (null? termlist) 
                (and (=zero? (coeff (car termlist)))
                     (zero-terms? (cdr termlist)))))]
    (zero-terms? (termlist poly))))

(put '=zero? 'polynomial =zero-poly?)
```

**2.88**

```
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in the same var -- SUB-POLY"
             (list p1 p2))))

(define (sub-terms l1 l2)
  (cond [(empty-termlist? l1) (map neg l2)]
        [(empty-termlist? l2) l1]
        [else 
         (let ((t1 (first-term l1))
               (t2 (first-term l2)))
           (cond [(> (order t1) (order t2))
                  (adjoin-term t1 (sub-terms (rest-terms l1) l2))]
                 [(< (order t1) (order t2))
                  (adjoin-term (neg t2) (sub-terms l1 (rest-terms t2)))]
                 [else 
                  (adjoin-term 
                   (make-term (order t1)
                              (sub (coeff t1) (coeff t2)))
                   (sub-terms (rest l1) (rest l2)))]))]))

(define (neg x)
  (apply-generic 'sub 0 x))
```
