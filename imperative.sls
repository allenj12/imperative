#!chezscheme
(library (imperative imperative)
    (export for
            while
            il
            :=
            return
            break!
            elif
            ::)
    (import (chezscheme))

(define-syntax :=
    (lambda (stx)
        (syntax-violation ':= "invalid use of keyword" stx)))

(define-syntax for
    (lambda (stx)
        (syntax-violation 'for "invalid use of keyword" stx)))

(define-syntax return
    (lambda (stx)
        (syntax-violation 'return "invalid use of keyword" stx)))

(define-syntax break!
    (lambda (stx)
        (syntax-violation 'break "invalid use of keyword" stx)))

(define-syntax elif
    (lambda (stx)
        (syntax-violation 'elif "invalid use of keyword" stx)))

(define-syntax bif
    (lambda (stx)
        (syntax-violation 'bif "invalid use of keyword" stx)))

(define-syntax while
    (lambda (stx)
        (syntax-violation 'while "invalid use of keyword" stx)))

(define-syntax ::
    (lambda (stx)
        (syntax-violation ':: "invalid use of keyword" stx)))

(meta define skip
    (lambda (stx)
        (syntax-case stx (= :: quote)
          [(:: n rest ...)
           (skip #'(rest ...))]
          [(= n rest ...)
           (skip #'(rest ...))]
          [(rest ...)
           #'(rest ...)])))


(define-syntax proc-brackets
    (lambda (stx)
        (syntax-case stx (quote = ::)
          [(_ v :: n = (quote #(e ...)) :: k rest ...)
           #'(vector-set! v n (proc-brackets (quote #(e ...)) :: k rest ...))]
          [(_ v :: n = a :: k rest ...)
           #'(vector-set! v n (proc-brackets a :: k rest ...))]
          [(_ v :: n = a rest ...)
           #'(vector-set! v n a)]
          [(_ v :: n :: rest ...)
           #'(proc-brackets (vector-ref v n) :: rest ...)]
          [(_ v :: n rest ...)
           #'(vector-ref v n)]
          [(_ rest)
           #'rest])))

(define-syntax weave
    (lambda (stx)
        (syntax-case stx ()
            [(_ (call arg (rest ...)))
            #'(call arg rest ...)])))

(define-syntax imp-lang
    (lambda (stx)
      (syntax-case stx (= := for if return break! else elif bif while :: quote)
        [(_ (c ... lc) return v :: n rest ...)
         #`(begin (lc (proc-brackets v :: n rest ...)) (weave (imp-lang (c ... lc) #,(skip #'(:: n rest ...)))))]
        [(_ (c ... lc) return n rest ...)
         #'(begin (lc n) (imp-lang (c ... lc) rest ...))]
        [(_ (c rc ...) break! rest ...)
         #'(begin (c (void)) (imp-lang (c rc ...) rest ...))]
        [(_ (c ...) (for (i := n) check step body ...) rest ...)
         #'(begin
            (call/cc 
                (lambda (nc)
                    (let for ([i n])
                        (when check
                            (begin
                                (imp-lang
                                    (nc c ...)
                                    body ...)
                            (for step))))))
            (imp-lang (c ...) rest ...))]
        [(_ (c ...) (while check body ...) rest ...)
         #'(begin
            (call/cc 
                (lambda (nc)
                    (let while ()
                        (when check
                            (begin
                                (imp-lang
                                    (nc c ...)
                                    body ...)
                            (while))))))
            (imp-lang (c ...) rest ...))]
        [(_ ccs (if ifcheck ifbody ...) (elif elcheck elbody ...) rest ...)
         #'(imp-lang ccs (bif (ifcheck (imp-lang ccs ifbody ...))
                              (elcheck (imp-lang ccs elbody ...))) rest ...)]
        [(_ ccs (if ifcheck ifbody ...) (else elsebody ...) rest ...)
         #'(begin 
            (if ifcheck
                (imp-lang ccs ifbody ...)
                (imp-lang ccs elsebody ...)) 
            (imp-lang ccs rest ...))]
        [(_ ccs (if ifcheck ifbody ...) rest ...)
         #'(begin
                (when ifcheck 
                      (imp-lang ccs ifbody ...))
                (imp-lang ccs rest ...))]
        [(_ ccs (bif checks ...) (elif elcheck elbody ...) rest ...)
         #'(imp-lang ccs (bif checks ... (elcheck (imp-lang ccs elbody ...))) rest ...)]
        [(_ ccs (bif checks ...) (else elsebody ...) rest ...)
         #'(begin (cond checks ... (else (imp-lang ccs elsebody ...))) 
                  (imp-lang ccs rest ...))]
        [(_ ccs (bif checks ...) rest ...)
         #'(begin (cond checks ...) 
                  (imp-lang ccs rest ...))]
        [(_ ccs v :: n rest ...)
         #`(begin (proc-brackets v :: n rest ...) (weave (imp-lang ccs #,(skip #'(:: n rest ...)))))]
        [(_ ccs x := v :: n rest ...)
         #`(let ([x (proc-brackets v :: n rest ...)]) (weave (imp-lang ccs #,(skip #'(:: n rest ...)))))]
        [(_ ccs x := n rest ...)
         #'(let ([x n]) (imp-lang ccs rest ...))]
        [(_ ccs x = v :: n rest ...)
         #`(begin (set! x (proc-brackets v :: n rest ...)) (weave (imp-lang ccs #,(skip #'(:: n rest ...)))))]
        [(_ ccs x = n rest ...)
         #'(begin (set! x n) (imp-lang ccs rest ...))]
        [(_ ccs h s rest ...)
         #'(begin h (imp-lang ccs s rest ...))]
        [(_ ccs h)
         #'h]
        [(_ ccs)
         #'(void)])))
         

(define-syntax il
    (lambda (stx)
        (syntax-case stx ()
        [(_ body ...)
        #'(call/cc
            (lambda (c) (imp-lang (c) body ...)))]))))

#| example qsort
(define partition
      (lambda (arr low high)
        (il
          pivot := arr :: high
          i := (- low 1)
          (for (j := low) (<= j (- high 1)) (+ j 1)
               arrj := arr :: j
               (if (< arrj pivot)
                   i = (+ 1 i)
                   temp := arr :: i
                   arr :: i = arr :: j
                   arr :: j = temp))
          temp := arr :: (+ 1 i)
          arr :: (+ 1 i) = arr :: high
          arr :: high = temp
          (+ i 1))))

(define qsort
    (lambda (arr low high)
      (il
        (if (< low high)
            pi := (partition arr low high)
            (qsort arr low (- pi 1))
            (qsort arr (+ pi 1) high)))))
|#
