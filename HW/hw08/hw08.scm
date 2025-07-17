(define (ascending? s) 
        (cond ((null? s) #t)
              ((null? (cdr s)) #t)
              ((> (car s) (car (cdr s))) #f)
              (else (ascending? (cdr s)))))

(define (my-filter pred s) 
        (if (null? s)
            nil
            (if (pred (car s)) 
                (cons (car s) (my-filter pred (cdr s)))
                (my-filter pred (cdr s)))))

(define (interleave lst1 lst2) 
        (cond ((and (null? lst1) (null? lst2)) nil)
              ((null? lst1) lst2)
              ((null? lst2) lst1)
              (else (cons (car lst1) (interleave lst2 (cdr lst1) )))))

(define (no-repeats s) 
        (if (null? s)
            s
            (cons (car s)
                  (no-repeats (filter (lambda (x) (not (= (car s) x))) (cdr s))))))