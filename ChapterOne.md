**1.30** 


```
(define (sum term a next b)
  (local [(define (iter a result)
            (if (> a b)
                result
                (iter (+ 1 a) (+ a result))))]
    (iter a 0)))
```

**1.31a**

```
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
```


**1.31b**
```
(define (product a b)
  (local [(define (iter a result)
            (if (> a b)
                result
                (iter (+ 1 a) (* a result))))]
    (iter a 1)))
```

**1.32**
```
(define (accumulate combiner null-value term a next b)
  (local [(define (iter a result)
            (if (> a b)
                result
                (iter (next a) (combiner a result))))]
    (iter a null-value)))
```

**1.33**
```
(define (filtered-accumulate combiner null-value term valid? a next b)
  (cond [(> a b) null-value]
        [(valid? a) (combiner (term a) 
                              (filtered-accumulate combiner null-value term valid? (next a) next b))]
        [else (combiner null-value 
                        (filtered-accumulate combiner null-value term valid? (next a) next b))]))
```

**1.41**
```
(define (double procedure)
  (λ (x) (procedure (procedure x))))
```

**1.42**
```
(define (compose1 proc1 proc2)
  (λ (x) (proc1 (proc2 x))))
```

**1.43**
```
(define (repeated proc n)
  (if (< n 1)
      (λ (x) x)
      (compose proc (repeated proc (sub1 n)))))
```
