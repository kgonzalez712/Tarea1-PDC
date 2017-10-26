;___________________________________MANEJO DE MATRICES_____________________________________________________
(define (createList size lista)
  (cond ((= 0 size)(list lista))
              (else (createList (- size 1) (cons 0 lista)))))

(define (createMat size)
  (cond ((= 0 size)'())
        (else (createMatAux '() size size))))


(define (createMatAux matrix size cant)
  (cond ((= 0 size)matrix)
        (else (createMatAux (append (createList cant '()) matrix) (- size 1) cant))))

;Da el valor indicado en la posicion indicada de la columna de la matriz
(define (setX List pos value)
  (setXAux List pos value '() 0)
  )
(define (setXAux List pos value newL index)
  (cond ((empty? List) newL)
        ((= pos index) (setXAux (cdr List) pos value (append newL (list value)) (+ index 1)))
        (else (setXAux (cdr List) pos value (append newL (list (car List))) (+ index 1)))
        )
  )

;Define el valor dado en la posicion x y de la matriz
(define (setVal matrix x y value)
  (setValAux matrix x y value '() 0)
  )
(define (setValAux matrix x y value newM index)
    (cond ((empty? matrix) newM)
        ((= index x) (setValAux (cdr matrix) x y value (append newM (list (setX (car matrix) y value))) (+ index 1)))
        (else (setValAux (cdr matrix) x y value (append newM  (list (car matrix))) (+ index 1)))
        )
  )

;toma el elemento de la posicion i de la lista
(define (getindex list i)
  (getindexAux list i list i)
  )
(define (getindexAux list i a ir)
  (cond ((= i 0) (car a))
        (else (getindexAux list (- i 1) (cdr a) ir)))
  )


(define (moves n)
  '((-2 -1) (-2 1) (-1 -2) (-1 2) (1 -2) (1 2) (2 -1) (2 1))
  )

(define (getele list ele)
  (getAux list ele 0)
  )
(define (getAux list ele i)
  (cond ((equal? list '()) -1)
        ((equal? (car list) ele) i)
        (else (getAux (cdr list) ele (+ i 1))))
  )

(define (getpos mat ele size)
  (getpAux mat ele size 0 '()))

(define (getpAux list ele size j pos)
  (cond ((not(equal? (getele (car list) ele) -1)) (cons j (cons (getele (car list) ele) pos)))
        ((equal? (car list) '()) -1)
        (else (getpAux (cdr list) ele size (+ j 1) pos)))
  )

;__________________________ Caballo ____________________________________________
;(define (PDC-Todas size pos)
  ;(cond())
;  )

(define (solve size matrix step i j movimientos n li)
  (cond((equal? step (* size size)) matrix)
       (else (myFor step size matrix i j movimientos 8 n li))
       )
  )
(define (myFor step size matrix x y sol n i li)
  (cond((equal? n 0) matrix)
       (else (myForAux step size (setVal matrix x y step) x y sol 8 i li))))

(define (myForAux step size matrix x y sol n i li)
  (cond ((equal? 8 i) (solve size (setVal matrix x  y 0) (- step 1)   (car (getpos matrix (- step 1) size)) (car (cdr(getpos matrix (- step 1) size))) sol (+ (car (cdr li)) 1) (cdr li)))

    ((isvalid? (+ x (getindex (getindex sol i) 0)) (+ y (getindex (getindex sol i) 1)) size matrix)
        (solve size (setVal matrix (+ x (getindex (getindex sol i) 0)) (+ y (getindex (getindex sol i) 1)) step) (+ step 1) (+ x (getindex (getindex sol i) 0))
               (+ y (getindex (getindex sol i) 1)) sol 0 (cons i li)))
       
       (else (myForAux step size matrix x y sol 8 (+ i 1) li))))



(define (isvalid? x y N matrix)
  (cond((and (and (and (<= 0 x) (< x N)) (and(<= 0 y) (< y N))) (equal? (getindex(getindex matrix x) y) 0)) #t)
       (else #f)
       )
  )

(define (isval? x x1 y y1)
  (cond ((checks (- x x1) (- y y1)) #t)
       (else #f)
       )
  )

(define (checks a b)
  (cond ((not(equal? (getele (moves 8) (cons a (cons b '()))) -1)) #t)
       (else #f)
       )
  )

(define (PDC-Test N sol)
  (Testsol sol '() 1 N)
  )
(define (Testsol sol mat i N)
  (cond ((equal? i (* N N)) (append mat(list(cons (car(getpos sol i N)) (cons (car(cdr(getpos sol i N))) '()))) ))
         ((isval? (car(getpos sol i N)) (car(getpos sol (+ i 1) N)) (car(cdr(getpos sol i N))) (car(cdr(getpos sol (+ i 1) N))))
          (Testsol sol (append mat(list(cons (car(getpos sol i N)) (cons (car(cdr(getpos sol i N))) '()))) ) (+ i 1) N))
         (else #f))
  )



(solve 5 (createMat 5) 1 2 2 (moves 8) 0 '(0))
