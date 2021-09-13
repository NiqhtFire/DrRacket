#lang racket
(require "teachpacks/evren-teachpack.rkt")
(define MERMİ (bitmap "imaj/mermi.png"))
(define DUSMAN (bitmap "imaj/dusman.png"))
(define OYUNCU (bitmap "imaj/karakter.png"))
;; STRUCT v - vektör
;; x : sayı - x koordinatı
;; y : sayı - y koordinatı
(STRUCT v( x y ))

(define (girişarkaplan) (bitmap "imaj/girişarkaplan.jpg"))
(define (bitişarkaplan) (bitmap "imaj/bitişarkaplan.jpg"))
;; v+ - vektör toplama
;; 
;;
;(ÖRNEK ....)
;(ÖRNEK ....)
(define(v+ v1 v2) (v (+ (v-x v1 )(v-x v2))(+(v-y v1) (v-y v2)) ))
(ÖRNEK (v+ (v 2 3) (v 4 5))(v 6 8))
(ÖRNEK (v+ (v -5 -8) (v 3 0))(v -2 -8))
(define karakter-genişliği(image-width (bitmap "imaj/karakter.png")))
(define karakter-yüksekliği(image-height (bitmap "imaj/karakter.png")))
;;;
;;içinde

;; ÇARPIŞMA
;;
;;
(define (çizgi-uzunluğu x y)(cond
                             [(< x y)(- y x)]
                             [else (- x y)]))
(define (degmeust n n1) (sqrt(+(sqr(çizgi-uzunluğu (v-x (nesne-yer n)) (v-x (nesne-yer n1))))(sqr(çizgi-uzunluğu (v-y (nesne-yer n)) (v-y (nesne-yer n1)))))))
(define (çarpıştı-mı? n n1 n2 n3 n4)
  (cond
    ((or ( < (degmeust n n1) 100) ( < (degmemermi n n2) 100) ( < (degmemermi n n3) 100) ( < (degmemermi n n4) 100)) (nesne (nesne-imaj n) (nesne-yer n) (v 1000 0) (v 0 0))  ) (else n)))





(define (degmemermi n n1) (sqrt(+(sqr(çizgi-uzunluğu (v-x (nesne-yer n)) (v-x (nesne-yer n1))))(sqr(çizgi-uzunluğu (v-y (nesne-yer n)) (v-y (nesne-yer n1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;

 ;MERMİ DÜŞMAN - DÜŞMAN YOk
;;;;;;;;;;;;;;;;;;;;;;;;

(define (evren-güncelle-genel e)
  (cond
    ((= (evren-aşama e) 0) e)
    ((= (evren-aşama e) 1) (evren-güncelle e))
    (else e)))


(define (çarpıştı-mı2? n n1)
  (cond
    (( < (degmemermi n n1) 60) (nesne (nesne-imaj n)  (v  (/ ekrangenisligi 2) (random -1500 -500)) (v 0 (random 25 30)) (v 0 0))  ) (else n)))




(define (çarp-yukarı n n1 )
  (cond [
         ( < (degmemermi n n1) 60) (nesne (nesne-imaj n)
         (v  (/ ekrangenisligi 2) (random 1000 1500)) (nesne-hız n) (nesne-ivme n))] [else n]))

(define (çarp-sol n n1 )
  (cond [
         ( < (degmemermi n n1) 60)  (nesne (nesne-imaj n)
         (v   (random 1600 1800) 360) (v (random 20 25) 0)(nesne-ivme n))] [else n]))


(define (çarp-sağ n n1 )
  (cond [
         ( < (degmemermi n n1) 60) (nesne (nesne-imaj n)
         (v (random -1000 -500) 360) (nesne-hız n)(nesne-ivme n))] [else n]))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (çarpıştı-mı-mermi n n1 )
  (cond
    (( < (degmemermi n n1) 60) (nesne (nesne-imaj n)  (v  (/ ekrangenisligi 2) (/ ekranyuksekligi 2)) (v 0 0) (v 0 0)) ) (else n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                              ;;;
;;;     MERMI SAĞ SOL                            ;;;
;;;                                              ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (alttan-sek n )
  (cond [
         (>= (+ (v-y (nesne-yer n)) (/ (image-height (nesne-imaj n)) 2 )) (image-height BACKGROUND)) (nesne (nesne-imaj n)
         (v (/ ekrangenisligi 2)(/ ekranyuksekligi 2)) (v 0 0)(nesne-ivme n))] [else n]))

(define (üst-sek n )
  (cond [
         (<= (- (v-y (nesne-yer n)) (/ (image-height (nesne-imaj n)) 2 )) 0) (nesne (nesne-imaj n)
         (v (/ ekrangenisligi 2)(/ ekranyuksekligi 2)) (v 0 0)(nesne-ivme n))] [else n]))

(define (sağ-sek n )
  (cond [
         (>= (+ (v-x (nesne-yer n)) (/ (image-width (nesne-imaj n)) 2 )) (image-width BACKGROUND)) (nesne (nesne-imaj n)
         (v (/ ekrangenisligi 2)(/ ekranyuksekligi 2)) (v 0 0)(nesne-ivme n))] [else n]))






(define (sol-sek n )
  (cond [
         (<= (- (v-x (nesne-yer n)) (/ (image-width (nesne-imaj n)) 2 )) 0) (nesne (nesne-imaj n)
         (v (/ ekrangenisligi 2)(/ ekranyuksekligi 2)) (v 0 0)(nesne-ivme n))] [else n]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                              ;;;
;;;                                              ;;;
;;;                                              ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                              ;;;
;;;                DÜŞMAN COLLIDER               ;;;
;;;                                              ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (d-asagi n )
  (cond [
         (>= (+ (v-y (nesne-yer n)) (/ (image-height (nesne-imaj n)) 2 )) (image-height BACKGROUND)) (nesne (nesne-imaj n)
         (v  (/ ekrangenisligi 2) (random -1500 -500)) (nesne-hız n)(nesne-ivme n))] [else n]))



(define (d-yukarı n )
  (cond [
         (<= (- (v-y (nesne-yer n)) (/ (image-height (nesne-imaj n)) 2 )) 0) (nesne (nesne-imaj n)
         (v  (/ ekrangenisligi 2) (random 1000 1500)) (nesne-hız n)(nesne-ivme n))] [else n]))


(define (d-sol n )
  (cond [
         (<= (- (v-x (nesne-yer n)) (/ (image-width (nesne-imaj n)) 2 )) 0)  (nesne (nesne-imaj n)
         (v   (random 1600 1800) 360) (nesne-hız n)(nesne-ivme n))] [else n]))

(define (d-sağ n )
  (cond [
         (>= (+ (v-x (nesne-yer n)) (/ (image-width (nesne-imaj n)) 2 )) (image-width BACKGROUND)) (nesne (nesne-imaj n)
         (v (random -1000 -500) 360) (nesne-hız n)(nesne-ivme n))] [else n]))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                              ;;;
;;;                                              ;;;
;;;                                              ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (randomla sayi sayi1)(random sayi sayi1))
;;;
;; v- - vektör çıkartma
;;  
;;
;(ÖRNEK ....)
;(ÖRNEK ....)
(define(v- v1 v2) (v (- (v-x v1 )(v-x v2))(-(v-y v1) (v-y v2)) ))
(ÖRNEK (v-(v -2 3) (v 3 2)) (v -5 1))
(ÖRNEK (v-(v 0 5) (v -5 2)) (v 5 3))
(ÖRNEK (v-(v -2 -3) (v 5 10)) (v -7 -13))
(ÖRNEK (v-(v 0 10) (v -5 -20)) (v 5 30))
;; v* - vektör sayıyla çarpma
;; 
;;
;(ÖRNEK ....)
;(ÖRNEK ....)
(define (v* sayi v1) (v (* (v-x v1) sayi) (* (v-y v1) sayi)))
(define (vsekmeçarp sayi v1)(v (v-x v1) (* (v-y v1) sayi)))
(ÖRNEK (v* 5 (v 5 5)) (v 25 25)) 
(ÖRNEK (v* -10 (v 3 20)) (v -30 -200))

;; v. - vektör dot çarpma
;;  
;;
;(ÖRNEK ....)
;(ÖRNEK ....)
(define (v. v1 v2)(+(* (v-x v1) (v-x v2)) (*(v-y v1)(v-y v2))))
(ÖRNEK (v. (v 5 5) (v -5 5)) 0)
(ÖRNEK (v. (v 10 5) (v 2 2)) 30)
;; v-mag - vektör uzunluğu
;; 
;;
;(ÖRNEK ....)
;(ÖRNEK ....)
(define(v-mag v1) (sqrt(+(sqr(v-x v1))(sqr(v-y v1)))))
(ÖRNEK(v-mag (v 6 8)) 10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hediye vektör çizim fonksiyonları
;; Vektör STRUCT tanıttıktan sonra bu fonkisyonları uncomment edebilirsiniz
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;place-image/v
; resim v sahne -> sahne
; bir sahneye vectöre göre bir imaj yerleştir
; template :
; (define (place-image/v im v1 sahne)
;  (... im ... (v-x v1) ... (v-y v1) ...)
(define test-circle (circle 10 "solid" "purple"))
(define test-square (square 100 "solid" "green"))

(ÖRNEK (place-image/v  test-circle (v 5 5) test-square)
       (place-image/align test-circle 5 5 "center" "center" test-square))
(ÖRNEK (place-image/v test-circle (v 3 8) test-square)
       (place-image/align test-circle 3 8 "center" "center" test-square))
(ÖRNEK (place-image/v test-circle (v 1 2) test-square)
       (place-image/align test-circle 1 2 "center" "center" test-square))
(ÖRNEK (place-image/v test-circle (v 2 8) test-square)
       (place-image/align test-circle 2 8 "center" "center" test-square))

(define (place-image/v im v1 sahne)
  (place-image/align im (v-x v1) (v-y v1) "center" "center"  sahne))

; place-line/v v v color görüntü -> görüntü
; v1'den v2'e giden bir çizgi arka imajına yerleştir
(ÖRNEK (place-line/v (v 2 3) (v 5 1) "red" test-square)
       (add-line test-square 2 3 5 1 "red")) 

(define (place-line/v v1 v2 renk arka)
  (add-line arka (v-x v1) (v-y v1) (v-x v2) (v-y v2) renk)) 

; place-text/v v metin sayı color görüntü -> görüntü
; v pozisyonda  verilen metni arka imajına yerleştir
(ÖRNEK (place-text/v (v 20 30) "Hello" 15 "red" test-square)
       (place-image/v (text "Hello" 15 "red") (v 20 30) test-square))
(define (place-text/v v metin size col arka)
  (place-image/v (text metin size col) v arka))


; STRUCT nesne
;; imaj : görüntü - nesneini imajı
;; yer : v - nesnenin ekrandaki yeri
;; hız : v - nesnenin hızı
;; ivme : v - nesnenin ivmesi
(STRUCT nesne (imaj yer hız ivme))

 
;;nesne-fizik-güncelle
(define (nesne-fizik-güncelle n) (nesne (nesne-imaj n) (v+ (nesne-yer n) (nesne-hız n)) (v+(nesne-hız n)(nesne-ivme n)) (nesne-ivme n)))

 
;;nesne-çiz 
(define (nesne-çiz n imaj) (place-image/v (nesne-imaj n) (nesne-yer n) imaj))
(ÖRNEK (nesne-çiz (nesne(circle 50 "solid" "red") (v 200 100) (v 5 10) (v 0 1)) (rectangle 400 200 "solid" "blue")) (place-image/v (circle 50 "solid" "red" ) (v 200 100) (rectangle 400 200 "solid" "blue")))
;; STRUCT evren
;; arkaplanı : görüntü - oyun arka planı
;;
(STRUCT evren (skor aşama arkaplan nesne-a nesne-b nesne-c mermi-1 mermi-2 mermi-3 dusman dusman-1 dusman-2 ))



(define skor 0)


(define (evren-güncelle e)
  (cond
    ((= (evren-aşama e) 0) (evren(evren-skor e)(evren-aşama e) (evren-arkaplan e) (evren-nesne-a e) (evren-nesne-b e) (evren-nesne-c e) (evren-mermi-1 e) (evren-mermi-2 e) (evren-mermi-3 e) (evren-dusman e) (evren-dusman-1 e) (evren-dusman-2 e)))
    ((= (evren-aşama e) 2) (evren(-(skor-güncelle e)(evren-skor e))
                                 (aşama-güncelle e)
                                 (evren-arkaplan e)
                                 (nesne (nesne-imaj (evren-nesne-a e)) (v (/ ekrangenisligi 2) (/ ekranyuksekligi 2)) (v 0 0) (v 0 0) )
                                 (evren-nesne-b e)
                                 (nesne (nesne-imaj (evren-nesne-c e)) (v (/ ekrangenisligi 2) (random -2000 -200)) (v 0 20) (v 0 0) )
                                 (evren-mermi-1 e)
                                 (evren-mermi-2 e)
                                 (evren-mermi-3 e)
                                 (nesne (nesne-imaj (evren-dusman e)) (v  (random 1600 1800)   360) (v -20 0) (v 0 0) )
                                 (nesne (nesne-imaj (evren-dusman-1 e)) (v (/ ekrangenisligi 2) (random 1000 1500))  (v 0 -20) (v 0 0) )
                                 (nesne (nesne-imaj (evren-dusman-2 e)) (v (random -1000 -500) 360) (v 20 0) (v 0 0) )))
    ((= (evren-aşama e) 1)
       
  (evren
   (skor-güncelle e)
   (aşama-güncelle e)
   (evren-arkaplan e)
   (çarpıştı-mı?(nesne-fizik-güncelle (evren-nesne-a e)) (evren-nesne-c e) (evren-dusman e) (evren-dusman-1 e)  (evren-dusman-2 e)) ;;nesne-a
   (çarpıştı-mı-mermi(sol-sek(nesne-fizik-güncelle (evren-nesne-b e))) (evren-dusman-2 e) ) ;;nesne-b
   (çarpıştı-mı2?(d-asagi(nesne-fizik-güncelle (evren-nesne-c e)))(evren-mermi-2 e)) ;;nesne-c
   (çarpıştı-mı-mermi(sağ-sek(nesne-fizik-güncelle(evren-mermi-1 e )))  (evren-dusman e)) ;;mermi-1
   (çarpıştı-mı-mermi(üst-sek(nesne-fizik-güncelle (evren-mermi-2 e)))  (evren-nesne-c e)) ;;mermi-2
   (çarpıştı-mı-mermi(alttan-sek(nesne-fizik-güncelle(evren-mermi-3 e)))(evren-dusman-1 e));; mermi-3 
  (çarp-sol(d-sol(nesne-fizik-güncelle(evren-dusman e)))(evren-mermi-1 e))
 (çarp-yukarı (d-yukarı (nesne-fizik-güncelle(evren-dusman-1 e)))(evren-mermi-3 e))
 (çarp-sağ (d-sağ (nesne-fizik-güncelle(evren-dusman-2 e)))(evren-nesne-b e))) )))


 


(define (evren-çiz e) 
  (cond 
    ((= (evren-aşama e) 0) (nesne-çiz (evren-nesne-a e) (girişarkaplan))) 
      ((= (evren-aşama e) 2)  (nesne-çiz (evren-nesne-a e) (bitişarkaplan)))
    ((= (evren-aşama e) 1)
                                                                                               
   (place-text/v (v 1100 20) (string-append "SKOR:" (number->string (evren-skor e))) 30 "white"
                 
     (nesne-çiz 
  (evren-dusman-2 e)
  (nesne-çiz (evren-dusman-1 e)
             (nesne-çiz (evren-dusman e)(nesne-çiz (evren-nesne-a e)
                                                   (nesne-çiz (evren-mermi-2 e)
                                                              (nesne-çiz (evren-mermi-1 e)
                                                                         (nesne-çiz  (evren-mermi-3 e)(nesne-çiz (evren-nesne-b e) (nesne-çiz (evren-nesne-c e) (evren-arkaplan e) )) ))))))))))) 
    
(define (skor-güncelle e)
  (cond
    [(< (degmeust (evren-mermi-1 e) (evren-dusman e)) 80) (+ (evren-skor e ) 10)]
     [(< (degmeust (evren-mermi-2 e) (evren-nesne-c e)) 80) (+ (evren-skor e ) 10)]
      [(< (degmeust (evren-mermi-3 e) (evren-dusman-1 e)) 80) (+ (evren-skor e ) 10)]
       [(< (degmeust (evren-nesne-b e) (evren-dusman-2 e)) 80) (+ (evren-skor e ) 10)] 
      (else (evren-skor e))))




(define (aşama-güncelle e)
  (cond
    [(< (degmeust (evren-nesne-a e) (evren-nesne-c e)) 100) 2]
     [(< (degmeust (evren-nesne-a e) (evren-dusman e)) 100) 2]
      [(< (degmeust (evren-nesne-a e) (evren-dusman-1 e)) 100) 2]
       [(< (degmeust (evren-nesne-a e) (evren-dusman-2 e)) 100) 2] 
      (else (evren-aşama e))))










(define (evren-fare e x y m)
  e)





(define (tus-vektor n t)
  (cond
    ((string=? t "left") (v -20 0)  )

    (else (nesne-hız n)) ) )

(define (tus-vektor1 n t)
  (cond
    ((and(string=? t "right") (not(string=? t "left"))) (v 20 0)  )

    (else(nesne-hız n) ) ))

(define (tus-vektor3 n t)
  (cond
    ((and(string=? t "up") (not(string=? t "down"))) (v 0 -20)   )

    (else(nesne-hız n) ) ))

(define (tus-vektor4 n t)
  (cond
    ((string=? t "down") (v 0 20) )     

    (else(nesne-hız n) ) ))


                       
(define BACKGROUND (bitmap "imaj/arkaplan.jpg")) 
(define ekranyuksekligi (image-height BACKGROUND))
(define FRAME-RATE 24)
(define ekrangenisligi (image-width BACKGROUND))
(define nesne1 (nesne (bitmap "imaj/karakter.png") (v (/ ekrangenisligi 2)  (/ ekranyuksekligi 2)) (v 0 0) (v 0 0)))


(define nesne2 (nesne  (bitmap "imaj/mermi.png") (v (/ ekrangenisligi 2)  (/ ekranyuksekligi 2)) (v 0 0) (v 0 0)))

(define mermi2 (nesne  (bitmap "imaj/mermi.png") (v (/ ekrangenisligi 2)  (/ ekranyuksekligi 2)) (v 0 0) (v 0 0)))
(define mermi3 (nesne  (bitmap "imaj/mermi.png") (v (/ ekrangenisligi 2)  (/ ekranyuksekligi 2)) (v 0 0) (v 0 0)))
(define mermi4 (nesne  (bitmap "imaj/mermi.png") (v (/ ekrangenisligi 2)  (/ ekranyuksekligi 2)) (v 0 0) (v 0 0)))

(define nesne3 (nesne  (bitmap "imaj/dusman.png") (v (/ ekrangenisligi 2) (random -2000 -200)) (v 0 20) (v 0 0))) ;;ÜST
(define dusman (nesne  (bitmap "imaj/dusman.png") (v  (random 1600 1800)   360) (v -20 0) (v 0 0)))  ;; SAĞ
(define dusman1 (nesne  (bitmap "imaj/dusman.png") (v (/ ekrangenisligi 2) (random 1000 1500)) (v 0 -20) (v 0 0))) ;;ALT
(define dusman2 (nesne  (bitmap "imaj/dusman.png") (v (random -1000 -500) 360) (v 20 0) (v 0 0))) ;; SOL


 



(define (hız-değiştir n v) (nesne (nesne-imaj n) (nesne-yer n) (v+ (nesne-hız n) v ) (nesne-ivme n)))


(define (evren-tuş e t)
(cond
  ((and (= (evren-aşama e) 0 ) (string=? t " ")) (evren
   (evren-skor e)                                               
   1 
   (evren-arkaplan e)
         (evren-nesne-a e)
         (nesne (nesne-imaj (evren-nesne-b e)) (nesne-yer (evren-nesne-b e)) (tus-vektor (evren-nesne-b e) t) (nesne-ivme (evren-nesne-b e)))
         (evren-nesne-c e)
         (nesne (nesne-imaj (evren-mermi-1 e)) (nesne-yer (evren-mermi-1 e)) (tus-vektor1 (evren-mermi-1 e) t) (nesne-ivme (evren-mermi-1 e)))
         (nesne (nesne-imaj (evren-mermi-2 e)) (nesne-yer (evren-mermi-2 e)) (tus-vektor3 (evren-mermi-2 e) t) (nesne-ivme (evren-mermi-2 e))) 
         (nesne (nesne-imaj (evren-mermi-3 e)) (nesne-yer (evren-mermi-3 e)) (tus-vektor4 (evren-mermi-3 e) t) (nesne-ivme (evren-mermi-3 e)))
          (evren-dusman e)
           (evren-dusman-1 e)
            (evren-dusman-2 e))
                                                 )  
  ((= (evren-aşama e) 1)
  (evren
   (evren-skor e)
   (evren-aşama e) 
   (evren-arkaplan e)
         (evren-nesne-a e)
         (nesne (nesne-imaj (evren-nesne-b e)) (nesne-yer (evren-nesne-b e)) (tus-vektor (evren-nesne-b e) t) (nesne-ivme (evren-nesne-b e)))
         (evren-nesne-c e)
         (nesne (nesne-imaj (evren-mermi-1 e)) (nesne-yer (evren-mermi-1 e)) (tus-vektor1 (evren-mermi-1 e) t) (nesne-ivme (evren-mermi-1 e)))
         (nesne (nesne-imaj (evren-mermi-2 e)) (nesne-yer (evren-mermi-2 e)) (tus-vektor3 (evren-mermi-2 e) t) (nesne-ivme (evren-mermi-2 e))) 
         (nesne (nesne-imaj (evren-mermi-3 e)) (nesne-yer (evren-mermi-3 e)) (tus-vektor4 (evren-mermi-3 e) t) (nesne-ivme (evren-mermi-3 e)))
          (evren-dusman e)
           (evren-dusman-1 e)
            (evren-dusman-2 e)))
  ((and (= (evren-aşama e) 2 ) (string=? t " ")) (evren
   (evren-skor e)                                               
   1 
   (evren-arkaplan e)
         (evren-nesne-a e)
         (nesne (nesne-imaj (evren-nesne-b e)) (nesne-yer (evren-nesne-b e)) (tus-vektor (evren-nesne-b e) t) (nesne-ivme (evren-nesne-b e)))
         (evren-nesne-c e)
         (nesne (nesne-imaj (evren-mermi-1 e)) (nesne-yer (evren-mermi-1 e)) (tus-vektor1 (evren-mermi-1 e) t) (nesne-ivme (evren-mermi-1 e)))
         (nesne (nesne-imaj (evren-mermi-2 e)) (nesne-yer (evren-mermi-2 e)) (tus-vektor3 (evren-mermi-2 e) t) (nesne-ivme (evren-mermi-2 e))) 
         (nesne (nesne-imaj (evren-mermi-3 e)) (nesne-yer (evren-mermi-3 e)) (tus-vektor4 (evren-mermi-3 e) t) (nesne-ivme (evren-mermi-3 e)))
          (evren-dusman e)
           (evren-dusman-1 e)
            (evren-dusman-2 e)) ) (else e)))   


(define yaradılış (evren skor 0 BACKGROUND nesne1 nesne2 nesne3 mermi2 mermi3 mermi4 dusman dusman1 dusman2 ))













;; SES herhangbirşey ses-dosyası-metin -> herhangibirşey
;; birinci paramatresini aynen dönsürüyor, sesi çalarak
(ÖRNEK (SES 0 "ses/bark.wav") 0)

(test)

(define (factorial n)
  (cond
    [(<= n 0) 1]
    [else (* n (factorial (- n 1)))]))
(define (daireler n)
  (cond
    [(<= n 0) (circle 5 "solid" "red")]
    [else (beside (circle (* 5 n) "solid" "red") (daireler (- n 1)))]))
(define (folder f null-v l)
  (cond
    [(empty? l) null-v]
    [else
     (f (first l)
          (folder f null-v (rest l)))]))

(STRUCT cons (first rest))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sabit kod bundan sonra                               ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yut (big-bang yaradılış
  (on-tick evren-güncelle (/ 1.0 FRAME-RATE))
  (on-draw evren-çiz)
  (on-key evren-tuş)
  (on-mouse evren-fare))) 

