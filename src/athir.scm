(define-syntax cond
    (syntax-rules (else =>)
        ((cond (else result1 result2 ...))
            (begin result1 result2 ...))
        ((cond (test => result))
            (let ((temp test))
                (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...)
            (let ((temp test))
                (if temp
                    (result temp)
                    (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...)
            (let ((temp test))
                (if temp
                    temp
                    (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...))
            (if test (begin result1 result2 ...)))
        ((cond (test result1 result2 ...)
                clause1 clause2 ...)
         (if test
            (begin result1 result2 ...)
            (cond clause1 clause2 ...)))))

(define-syntax let 
    (syntax-rules ()
        ((let ((name value) ...) body ...)
            ((lambda (name ...) body ...) value ...))))

(define-syntax let*
    (syntax-rules ()
        ((let* () body1 body2 ...)
            (let () body1 body2 ...))
        ((let* ((name1 val1) (name2 val2) ...)
            body1 body2 ...)
            (let ((name1 val1))
                (let* ((name2 val2) ...)
                    body1 body2 ...)))))

(define-syntax and
    (syntax-rules ()
        ((and) #t)
        ((and a) a)
        ((and a b ...)
            (if a (and b ...) #f))))

(define-syntax or
    (syntax-rules ()
        ((or) #f)
        ((or a) a)
        ((or a b ...)
            (if a a (or b ...)))))

(define-syntax when
    (syntax-rules ()
        ((when test result1 result2 ...)
            (if test
                (begin result1 result2 ...)))))

(define-syntax unless
    (syntax-rules ()
        ((unless test result1 result2 ...)
            (if (not test)
                (begin result1 result2 ...)))))

(define-syntax begin
    (syntax-rules ()
        ((begin exp ...)
        ((lambda () exp ...)))))
