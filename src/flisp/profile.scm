(define (string.rpad s n c) (string s (string.rep c (- n (string.count s)))))

(define (string.rep s k)
  (cond ((< k 4)
         (cond ((<= k 0) "")
               ((=  k 1) (string s))
               ((=  k 2) (string s s))
               (else     (string s s s))))
        ((odd? k) (string s (string.rep s (- k 1))))
        (else     (string.rep (string s s) (/ k 2)))))

(let ((*profiles* (table))
      (running #f))
  (set! profile
        (lambda (s)
          (let ((f (top-level-value s)))
            (put! *profiles* s (cons 0 (cons 0 0))) ; count, self, total
            (set-top-level-value! s
             (lambda args
               (define tt (get *profiles* s))
               (define last-tt running)
               (define last-t0 (cddr tt))
               (define t0 (time.now))
               (set! running tt)
               (define v (apply f args))
               (set! running last-tt)
               (define t1 (time.now))
               (define tdelta (- t1 t0))
               (if last-tt (set-car! (cdr last-tt) (- (cadr last-tt) tdelta)))
               (set-car! (cdr tt) (+ (cadr tt) tdelta))
               (set-cdr! (cdr tt) (+ last-t0 tdelta))
               (set-car! tt (+ (car tt) 1))
               v)))))
  (set! show-profiles
        (lambda ()
          (define total 0)
          (define pr (filter (lambda (x) (> (cadr x) 0))
                             (table.pairs *profiles*)))
          (define width (+ 4
                           (apply max
                                  (map (lambda (x)
                                         (length (string x)))
                                       (cons 'Function
                                             (map car pr))))))
          (princ (string.rpad "Function" width #\ )
                 "#Calls     Total Time (seconds)    Self Time (seconds)")
          (newline)
          (princ (string.rpad "--------" width #\ )
                 "------     --------------------    -------------------")
          (newline)
          (for-each
           (lambda (p)
         (set! total (+ total (cadr p)))
             (princ (string.rpad (string (cadddr p)) width #\ )
                    (string.rpad (string (caddr p)) 11 #\ )
                    (string.rpad (string (car p)) 24 #\ )
            (cadr p))
             (newline))
           (reverse (simple-sort (map (lambda (l) (reverse (to-proper l)))
                                      pr))))
       (princ (string.rpad "--------" width #\ )
                 "------     --------------------    -------------------")
       (newline)
       (princ (string.rpad "Total   " width #\ )
                 "           " (string total))
       (newline)))
  (set! clear-profiles
        (lambda ()
          (for-each (lambda (k)
                      (put! *profiles* k (cons 0 (cons 0 0))))
                    (table.keys *profiles*)))))

