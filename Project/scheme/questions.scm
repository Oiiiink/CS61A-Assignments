(define (let-to-lambda expr)
  (cond ((atom?   expr) expr)
        ((eq? (car expr) 'quoted) expr)
        ((eq? (car expr) 'lambda) 
                        (cons 'lambda 
                              cons((car (cdr expr)) (let-to-lambda (cdr (cdr expr))))))
        ((eq? (car expr) 'define)
                        (cons 'define
                              cons((car (cdr expr)) (let-to-lambda (cdr (cdr expr))))))
        ((eq? (car expr) 'let) 
                        (let ((name (car (zip (car (cdr expr)))))
                              (para (car (cdr (zip (car (cdr expr)))))))
                              (cons 'lambda
                                    (cons name 
                                        (cons (let-to-lambda (cdr (cdr expr)))
                                                (cons para nil))))))
        (else           expr)))