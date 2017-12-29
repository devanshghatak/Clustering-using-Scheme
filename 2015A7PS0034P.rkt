#lang racket

(provide (all-defined-out))
(define input (file->list (vector-ref (current-command-line-arguments) 0)))
;; read args from command line

;;(define file (open-input-file path))

(define N (list-ref input 0))
(define D (list-ref input 1))
(define K (list-ref input 2))
(define E (list-ref input 3))
(define MP (list-ref input 4))
;(define enter (read))
(define (myreadr lst n i)
	(if (> n 0) (myreadr (cons (list-ref input i) lst) (- n 1) (+ 1 i)) (reverse lst))
)
(define (myRead lst n i)
	(if (> n 0) (myRead (cons (myreadr (list) D i) lst) (- n 1) (+ D i)) (reverse lst))
)
(define points  (myRead (list) N 5))


;(newline)
;(display N)
;(newline)
;(display D)
;(newline)
;(display K)
;(newline)
;(display E)
;(newline)
;(display MP)
;(newline)
;(display points)
;(newline)

(define (mkpoints lstp k)
	(if (<= k N) (mkpoints (cons (list k (list-ref points (- k 1))) lstp) (+ k 1)) (reverse lstp))
	)
(define step1 (mkpoints (list) 1))
;(display step1)
;(newline)
;; 1.a

(define (eucd sum li lj)
	(if (null? li) sum (eucd (+ (* (- (car li) (car lj)) (- (car li) (car lj))) sum) (cdr li) (cdr lj)))
)
(define (eucdR lstr i j) (if (= i j) (eucdR (cons +inf.0 lstr)  i (+ j 1))
	(if (= j N) (reverse lstr) (eucdR (cons (sqrt (eucd 0 (list-ref points i) (list-ref points j))) lstr)  i (+ j 1)))
	)
)
(define (mat lstm n)
	(if (= n N) (reverse lstm) (mat (cons (eucdR (list) n 0) lstm) (+ n 1)))
)
(define spMat (mat (list) 0))

;(display spMat)

(define (mkmat lstk p)
	(if (<= p N) (mkmat (cons (mkmatr (list) p 1) lstk) (+ p 1)) (reverse lstk))
	)

(define (mkmatr lstr p r)
	(if (<= r N) (mkmatr (cons (list r (list-ref (list-ref spMat (- p 1)) (- r 1))) lstr) p (+ r 1)) (reverse lstr))
	)

(define step2p (mkmat (list) 1))

(define precision '6)

(define (mysetprecision n p)
  (if (= n +inf.0) +inf.0
      (string->number (~r n #:precision p))
  )
) 

(define (precision_util lst)
  (if (null? lst) '()
      (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst))))
)

(define (modify_precision lst)
  (if (null? lst) '()
  (cons (precision_util (car lst)) (modify_precision (cdr lst))))
)
(define step2 (modify_precision step2p))
;(newline)
;(display step2)

;; 1.b
(define (comp? a b)
	(<= (list-ref a 1) (list-ref b 1))
)
(define (matSort lsts matr n)
	(if (= n N) (reverse lsts) (matSort (cons (sort (car matr) comp?) lsts) (cdr matr) (+ n 1)))
)

(define spMatSorted (matSort (list) step2 0))
;(newline)
;(newline)
;(display spMatSorted)

(define (getKNNr lstr row k)
	(if (= k K) (reverse lstr) (getKNNr (cons (car row) lstr) (cdr row) (+ k 1)))
)
(define (getKNN lsts matr n)
	(if (= n N) (reverse lsts) (getKNN (cons (getKNNr (list) (car matr) 0) lsts) (cdr matr) (+ n 1)))
)
(define KNN (getKNN (list) spMatSorted 0))
;(newline)
;(newline)
;(display KNN)

(define (make3r lsts row)
	(if (null? row) (reverse lsts) (make3r (cons (car (car row)) lsts) (cdr row))))
(define (make3 lsts matr n)
	(if (= n N) (reverse lsts) (make3 (cons (make3r (list) (car matr)) lsts) (cdr matr) (+ n 1))))

(define step3p (make3 (list) KNN 0))
;(newline)
;(newline)
;(display step3p)

(define (sort3 lsts matr n)
	(if (= n N) (reverse lsts) (sort3 (cons (sort (car matr) <) lsts) (cdr matr) (+ n 1))))

(define step3 (sort3 (list) step3p 0))
;(newline)
;(newline)
;(display step3)

(define (mycheck? ref i)
	(if (null? ref) (= 1 0) (if (= i (car ref)) (= 0 0) (mycheck? (cdr ref) i)) )
)
(define (getSharedr lsts row i)
	(if (null? row) (reverse lsts) (if (mycheck? (list-ref step3 (- (car row) 1)) i) (getSharedr (cons (car row) lsts) (cdr row) i) (getSharedr lsts (cdr row) i)))
)
(define (getShared lsts matr n)
	(if (= n N) (reverse lsts) (getShared (cons (getSharedr (list) (car matr) (+ n 1)) lsts) (cdr matr) (+ n 1)))
)

(define shared (getShared (list) step3 0))
;(newline)
;(newline)
;(display shared)

(define (weigh sum row knn)
	(if (null? row) sum (if (mycheck? knn (car row)) (weigh (+ sum 1) (cdr row) knn) (weigh (+ sum 0) (cdr row) knn)))
)
(define (weighr lsts row n)
	(if (null? row) (reverse lsts) (weighr (cons (list (car row) (weigh 0 (list-ref step3 n) (list-ref step3 (- (car row) 1)))) lsts) (cdr row) n))
)
(define (weight lsts matr n)
	(if (= n N) (reverse lsts) (weight (cons (weighr (list) (car matr) n) lsts) (cdr matr) (+ n 1)))
)

(define weighed (weight (list) shared 0))
;(newline)
;(newline)
;(display weighed)

(define (comp1? a b)
	(> (list-ref a 1) (list-ref b 1))
)
(define (matSort1 lsts matr n)
	(if (= n N) (reverse lsts) (matSort1 (cons (sort (car matr) comp1?) lsts) (cdr matr) (+ n 1)))
)
(define step4 (matSort1 (list) weighed 0))
;(newline)
;(newline)
;(display step4)

(define (coreSum sum row)
	(if (null? row) sum (if (<= E (car (cdr (car row)))) (coreSum (+ sum 1) (cdr row)) (coreSum (+ sum 0) (cdr row))))
)
(define (getCore lsts matr n)
	(if (= n N) (reverse lsts) (getCore (cons (coreSum 0 (car matr)) lsts) (cdr matr) (+ n 1)))
)

(define step5 (getCore (list) step4 0))
;(newline)
;(newline)
;(display step5)

(define (getCorePts lsts n)
	(if (= n N) (reverse lsts) (if (>= (list-ref step5 n) MP) (getCorePts (cons (+ n 1) lsts) (+ n 1)) (getCorePts lsts (+ n 1))))
)
(define step6 (getCorePts (list) 0))
;(newline)
;(newline)
;(display step6)

(define (idNoise lsts n)
	(if (= n N) (reverse lsts) (if (and (not (mycheck? step6 (+ n 1))) (= 0 (list-ref step5 n))) (idNoise (cons (+ n 1) lsts) (+ n 1)) (idNoise lsts (+ n 1))))
)
(define step8 (idNoise (list) 0))
;(newline)
;(newline)
;(display step8)

(define (idBorder lsts n)
	(if (= n N) (reverse lsts) (if (and (not (mycheck? step6 (+ n 1))) (not (mycheck? step8 (+ n 1)))) (idBorder (cons (+ n 1) lsts) (+ n 1)) (idBorder lsts (+ n 1))))
)
(define step9 (idBorder (list) 0))
;(newline)
;(newline)
;(display step9)
(define step7 0)
(define step10 0)


#|
;;****TEMPLATE****;;

(define (myfunc lst)
    (cond
        (null? lst)

        (void)

        (else
        	;;Do what you want
        	(myfunc (rest lst)))
    )
)
|#