;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname submission) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
; Fundies 1 Homework 10 - David Malone, Cameron Boggio

; Ex 3

(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

; A Feature is a
; - WholeNumber
; Representing the grey-scale value (from 0 to 255) of a single pixel    
(define F-0 0)
(define F-4 4)
(define F-156 156)
(define F-255 255)

; bitmap->image : Bitmap -> Image
; Creates an image based on a given bitmap
(check-expect (bitmap->image (list (list 255)))
              (feature->image 255))
(check-expect (bitmap->image (list
                              (list 200 255)
                              (list 255 0)))
              (above
               (beside (feature->image 200) (feature->image 255))
               (beside (feature->image 255) (feature->image 0))))
(check-expect (bitmap->image (list
                              (list 200 255 205)
                              (list 255 0   213)
                              (list 252 255 105)))
              (above
               (beside (feature->image 200) (feature->image 255) (feature->image 205))
               (beside (feature->image 255) (feature->image 0)   (feature->image 213))
               (beside (feature->image 252) (feature->image 255) (feature->image 105))))
(define (bitmap->image bm)
  (local [; string-together -> [List-of Image] -> [Image]
          ; Take a list of images, convert them to pixels, and then string
          ; those together
          (define (string-together loi)
            (foldr beside empty-image (map feature->image loi)))
          ]
    (foldr above empty-image (map string-together bm))))

; A ListOfFeatures (LoF) is one of: 
; - empty
; - (cons Feature LoF)
; and represents a list of Features
(define LOF-0 empty)
(define LOF-1 (cons F-0 LOF-0))
(define LOF-2 (cons F-4 LOF-1))
(define LOF-3 (cons F-156 LOF-2))
(define LOF-4 (cons F-255 LOF-3))
(define LOF-3A (cons F-156 LOF-2))
(define LOF-3B (cons F-4 LOF-2))

(define (lof-temp lof)
  (...
   (cond [(empty? lof)... ]
         [(cons? lof)...
          (first lof)...
          (lof-temp (rest lof))... ])))

; A Bitmap is one of : 
; - empty
; - (cons LoF Bitmap)
; representing an entire image via the rows and columns of Features in a grid.
(define BM-0 empty)
(define BM-1 (cons LOF-1 BM-0))
(define BM-2 (cons LOF-2 BM-1))
(define BM-3 (cons LOF-3 BM-2))
(define BM-4 (cons LOF-4 BM-3))
(define BM-ACTUAL (cons LOF-3 (cons LOF-3A (cons LOF-3B empty))))  ; represents a 3x3 grid

(define (bitmap-temp bm)
  (...
   (cond [(empty? bm)... ]
         [(cons? bm)...
          (lof-temp (first bm))...
          (bitmap-temp (rest bm))... ])))

(define BM (list
            (list 200 255 205)
            (list 255 0   213)
            (list 252 255 105)))

;; An Instance is a ListOfFeatures
;; and represents a list of grayscale pixels
(define INSTANCE-EMPTY empty)
(define INSTANCE-ZERO
  (cons 254
        (cons 255
              (cons 255
                    (cons 255
                          (cons 0
                                (cons 253
                                      (cons 252
                                            (cons 255
                                                  (cons 255 empty))))))))))

(define-struct training [fname inst img digit])
;; A Training is a (make-training String Instance Image Integer)
;; - where fname is the file name or "" if the data did not come from a file
;; - inst is the flattened list of pixels
;; - img is the visualization of the bitmap of pixels
;; - and digit is the digit that this is an image of

(define PIXEL-SIZE 10)
(define IMAGE-ZERO
  (above
   (beside
    (square PIXEL-SIZE "solid" "black")
    (square PIXEL-SIZE "solid" "black")
    (square PIXEL-SIZE "solid" "black"))
   (beside
    (square PIXEL-SIZE "solid" "black")
    (square PIXEL-SIZE "solid" "white")
    (square PIXEL-SIZE "solid" "black"))
   (beside
    (square PIXEL-SIZE "solid" "black")
    (square PIXEL-SIZE "solid" "black")
    (square PIXEL-SIZE "solid" "black"))))
(define TRAINING-0
  (make-training "" INSTANCE-ZERO IMAGE-ZERO 0))

;; training-template : Training -> ???
(define (training-template t)
  (... (training-fname t) ...
       (instance-template (training-inst t)) ...
       (training-img t) ...
       (training-digit t) ...))

(define-struct testing [fname inst img])
;; A Testing is a (make-testing String Instance Image)
;; - where fname is the name of the file the data came from or "" if it is not from a file
;; - inst is the flattened list of pixels
;; - and img is the visualization of the bitmap of pixels

(define TESTING-ZERO (make-testing "" INSTANCE-ZERO IMAGE-ZERO))

;; testing-template : Testing -> ???
(define (testing-template t)
  (... (testing-fname t) ...
       (instance-template (testing-inst t)) ...
       (testing-img t) ...))

(define-struct neighbor [training dist])
;; A Neighbor is a (make-neighbor Training NonNegNumber)
;; - where training is an instance of training data
;; - and dist is the "distance" from a certain testing data to that training data

(define NEIGHBOR-ZERO (make-neighbor TRAINING-0 3))
(define NEIGHBOR-1 (make-neighbor TRAINING-0 1))
(define NEIGHBOR-2 (make-neighbor TRAINING-0 2))
(define NEIGHBOR-4 (make-neighbor TRAINING-0 4))
(define NEIGHBOR-5 (make-neighbor TRAINING-0 5))

;; neighbor-template : Neighbor -> ???
(define (neighbor-template n)
  (... (training-template (neighbor-training n)) ...
       (neighbor-dist n) ...))

(define-struct worldstate [index nneighbor])
; A WorldState is a (make-worldstate NatNum Neighbor [List-of Training]) in which
; - index represents the index in a [List-of Training] and
; - nneighbor represents the closest Neighbor
(define WS-0 (make-worldstate 0 NEIGHBOR-ZERO))
(define WS-1 (make-worldstate 0 NEIGHBOR-1))
(define WS-2 (make-worldstate 2 NEIGHBOR-2))
(define (ws-temp ws)
  (...(worldstate-index ws)
      (worldstate-digit ws)...))

; mnist : Number String -> Number
; Runs a world program displaying the similarity of written digits
; and returning the most similar digit
(define (mnist num str)
  (local [; file->training : String -> Training
          ; Converts a filename to a Training
          ; If given d_1_0.txt, returns (make-training "d_1_0.txt"
          ;                                            (flatten (read-lolon "d_1_0.txt"))
          ;                                            (file->image "d_1_0.txt")
          ;                                            (fname->label "d_1_0.txt"))
          (define (file->training s)
            (make-training s (flatten (read-lolon s)) (file->image s) (fname->label s)))
          (define TRAINING-LIST (map file->training (training-fnames num)))
          (define NEIGHBOR-LIST
            (if (> num 30)
                (error "Out of bounds")
                (map (λ (training) (make-neighbor training (euclidean-dist
                                                            (training-fname training)
                                                            str)))
                     TRAINING-LIST)))
          (define PREDICTION (nearest-neighbor NEIGHBOR-LIST))]
    (training-digit (neighbor-training
                     (worldstate-nneighbor
                      (big-bang (make-worldstate 0 PREDICTION)
                        [to-draw (λ (ws) (draw-frame ws str NEIGHBOR-LIST))]
                        [on-key (λ (ws ke) (update-worldstate ws ke NEIGHBOR-LIST))]))))))

; draw-frame : WorldState String [List-of Neighbors] -> Image
; Draws an image demonstrating 
(define (draw-frame ws fpath nlist)
  (beside (above (text "Test Image" 18 "red")
                 (file->image fpath)              
                 (text (string-append "Best Match: "
                                      (number->string (training-digit
                                                       (neighbor-training
                                                        (worldstate-nneighbor ws)))))
                       18
                       "purple")
                 (text (number->string (neighbor-dist (worldstate-nneighbor ws)))
                       18
                       "purple")
                 (file->image (training-fname (neighbor-training (worldstate-nneighbor ws)))))
          (above (text "Training" 18 "blue")
                 (text (string-append (number->string (training-digit
                                                       (neighbor-training
                                                        (list-ref nlist
                                                                  (worldstate-index ws)))))
                                      ": "
                                      (number->string
                                       (neighbor-dist
                                        (list-ref nlist
                                                  (worldstate-index ws)))))
                       18
                       "blue")                       
                 (training-img (neighbor-training (list-ref nlist (worldstate-index ws)))))))

; update-worldstate : WorldState KeyEvent [List-of Neighbors] -> WorldState
; Updates the WorldState by correctly changing the index
(check-expect (update-worldstate WS-1 "right" (list TRAINING-0 TRAINING-0 TRAINING-0 TRAINING-0))
              (make-worldstate
               1
               (worldstate-nneighbor WS-1)))
(check-expect (update-worldstate WS-2 "left" (list TRAINING-0 TRAINING-0 TRAINING-0 TRAINING-0))
              (make-worldstate
               1
               (worldstate-nneighbor WS-2)))
(check-expect (update-worldstate WS-2 "r" (list TRAINING-0 TRAINING-0 TRAINING-0 TRAINING-0))
              WS-2)
(define (update-worldstate ws ke nlist)
  (cond [(key=? ke "right") (make-worldstate
                             (next-index nlist (worldstate-index ws))
                             (worldstate-nneighbor ws))]
        [(key=? ke "left") (make-worldstate
                            (prev-index nlist (worldstate-index ws))
                            (worldstate-nneighbor ws))]
        [else ws]))

; training-fnames : Nat -> [List-of String]
; Returns a list of file paths, following the pattern "train/d_i_j.txt", where
; - j is {0, 1, ... 9}
; - i is {1, 2, ... n} (where n is a supplied number of examples per digit).
(check-expect
 (training-fnames 3)
 (list "train/d_1_0.txt" "train/d_2_0.txt" "train/d_3_0.txt"
       "train/d_1_1.txt" "train/d_2_1.txt" "train/d_3_1.txt"
       "train/d_1_2.txt" "train/d_2_2.txt" "train/d_3_2.txt"
       "train/d_1_3.txt" "train/d_2_3.txt" "train/d_3_3.txt"
       "train/d_1_4.txt" "train/d_2_4.txt" "train/d_3_4.txt"
       "train/d_1_5.txt" "train/d_2_5.txt" "train/d_3_5.txt"
       "train/d_1_6.txt" "train/d_2_6.txt" "train/d_3_6.txt"
       "train/d_1_7.txt" "train/d_2_7.txt" "train/d_3_7.txt"
       "train/d_1_8.txt" "train/d_2_8.txt" "train/d_3_8.txt"
       "train/d_1_9.txt" "train/d_2_9.txt" "train/d_3_9.txt"))
(check-expect
 (training-fnames 2)
 (list "train/d_1_0.txt" "train/d_2_0.txt"
       "train/d_1_1.txt" "train/d_2_1.txt"
       "train/d_1_2.txt" "train/d_2_2.txt"
       "train/d_1_3.txt" "train/d_2_3.txt"
       "train/d_1_4.txt" "train/d_2_4.txt"
       "train/d_1_5.txt" "train/d_2_5.txt"
       "train/d_1_6.txt" "train/d_2_6.txt"
       "train/d_1_7.txt" "train/d_2_7.txt"
       "train/d_1_8.txt" "train/d_2_8.txt"
       "train/d_1_9.txt" "train/d_2_9.txt"))
(check-expect
 (training-fnames 1)
 (list "train/d_1_0.txt"
       "train/d_1_1.txt"
       "train/d_1_2.txt"
       "train/d_1_3.txt"
       "train/d_1_4.txt"
       "train/d_1_5.txt"
       "train/d_1_6.txt"
       "train/d_1_7.txt"
       "train/d_1_8.txt"
       "train/d_1_9.txt"))

(define (training-fnames i)
  (foldr append '() (build-list (add1 9) (λ (j) (create-i-list i j)))))

; create-i-list : Nat Nat -> [List-of String]
; Makes a list of increasing i values from [1, i] with a static j
(check-expect (create-i-list 2 0) (list "train/d_1_0.txt" "train/d_2_0.txt"))
(check-expect (create-i-list 2 1) (list "train/d_1_1.txt" "train/d_2_1.txt"))
(check-expect (create-i-list 0 0) empty)
(define (create-i-list i j)
  (build-list i (λ (x) (string-append "train/d_"
                                      (number->string (add1 x))
                                      "_"
                                      (number->string j)
                                      ".txt"))))

; fname->label : String -> Integer
; Returns the last number in a file name (a filename ends with one 1-digit number, a period,
; and a 3-character extension)
(check-expect (fname->label "train/d_1_0.txt") 0)
(check-expect (fname->label "meme_0.rkt") 0)
(check-expect (fname->label "hello2.jpg") 2)
(check-expect (fname->label "h3ll0_fr13nd_9.cpp") 9)
(define (fname->label fname)
  (string->number (substring fname (- (string-length fname) 5) (- (string-length fname) 4))))

; read-lolon : String -> [List-of [List-of Number]]
; Produces a [List-of [List-of Number]] from a matrix in a file
(check-expect (read-lolon "numbers1.txt")
              (list
               (list 0 0 0 0)
               (list 1 2 3 4)
               (list 2 1 0 -0.5)))
(check-expect (read-lolon "numbers2.txt")
              (list
               (list 21 32 81 79)
               (list 100 100 100 100)))
(check-expect (read-lolon "numbers3.txt")
              (list
               (list 43 15 26 134)
               (list 94 17 139 19)
               (list 169 171 4 18)
               (list 17 187 183 4)))

(define (read-lolon f)
  (map (λ (x) (map string->number x)) (read-words/line f)))

; flatten : (X) [List-of [List-of X]] -> [List-of X]
; Converts a list of lists of a given type into a single list
; of that given type

(check-expect (flatten (list
                        (list "a" "b" "c")
                        (list "d" "e" "f")))
              (list "a" "b" "c" "d" "e" "f"))
(check-expect (flatten (list (list 1 2 3 4 0)
                             (list 1 2 3 4)))
              (list 1 2 3 4 0 1 2 3 4))
(check-expect (flatten (list
                        (list "#true" "#true" "#true")
                        (list "#false" "#true" "#false")))
              (list "#true" "#true" "#true" "#false" "#true" "#false"))

(define (flatten list)
  (foldr append empty list))

; feature->image : Feature -> Image
; Creates a square of the color of the feature
(check-expect (feature->image 0)
              (square 10 "solid" (make-color 255 255 255)))
(check-expect (feature->image 255)
              (square 10 "solid" (make-color 0 0 0)))
(check-expect (feature->image 15)
              (square 10 "solid"  (make-color 240 240 240)))

(define (feature->image f)
  (local [; white->black : Feature -> Feature
          ; flip the color of the given feature
          (define (white->black f_prime)
            (- 255 f_prime))
          ; feature->color : Feature -> Color
          ; Creates a grey pixel
          (define (feature->color f_prime)
            (make-color (white->black f_prime)
                        (white->black f_prime)
                        (white->black f_prime)))
          ]
    (square 10 "solid" (feature->color f))))

; map-2list : (X Y) [List-of X] [List-of X] [X X -> Y] -> [Or Error [List-of Y]]
; maps the function to each element in both lists
(check-error (map-2list (list 1 2) (list 3 4 5 6) +)
             "Lists cannot be of different lengths.")
(check-expect (map-2list (list 1 2 3) (list 4 5 6) +)
              (list 5 7 9))
(check-expect (map-2list (list "a" "b" "c") (list "d" "e" "f") string-append)
              (list "ad" "be" "cf"))
(check-error (map-2list (list "a" "b" "c") '() string-append)
             "Lists cannot be of different lengths.")

(define (map-2list list1 list2 func)
  (if (not (= (length list1) (length list2)))
      (error "Lists cannot be of different lengths.")
      (cond [(empty? list1) '()]
            [(cons? list1)
             (cons (func (first list1) (first list2))
                   (map-2list (rest list1) (rest list2) func))])))

; smallest-of-list-by-f : (X Y) [X -> Y] [List-of X] -> X
; Finds the first element in a non-empty list that minimizes a supplied function

(check-expect
 (smallest-of-list-by-f length
                        (list
                         (list 1 2 3)
                         (list 100)
                         (list -1000 -99 -1 0)
                         (list 2)))
 (list 100))
(check-expect
 (smallest-of-list-by-f string-length
                        (list
                         "apple"
                         "b"
                         "a"
                         "applesauce"
                         "apple pie"))
 "b")
(check-expect
 (smallest-of-list-by-f abs
                        (list
                         -123
                         8
                         -17
                         2
                         81))
 2)
(define (smallest-of-list-by-f func list) 
  (local [; find-min : (X) X X -> X
          ; find the smaller value of two values when a function is called on them
          (define (find-min x y)
            (if (< (func x) (func y))
                x
                y))]              
    (foldl find-min (first list) list)))

(define L-1 (list 1 2 3 4))
(define L-2 (list 0 1 2 3 4))
(define L-3 (list "a" "b" "c" "d"))
(define L-4 (list L-1 L-2))

; next-index : (X) [List-of X] Nat -> X
; Returns the next valid index in a list (or 0 if the supplied index is the last valid index)
(check-expect (next-index L-1 1) 2)
(check-expect (next-index L-2 2) 3)
(check-expect (next-index L-3 3) 0)
(check-expect (next-index L-4 0) 1)
(check-expect (next-index '() 2) 0)

(define (next-index list i)
  (cond [(empty? list) 0]
        [(cons? list) (if (> (add1 i) (sub1 (length list)))
                          0
                          (add1 i))]))


; prev-index : (X) [List-of X] Nat -> X
; Returns the previous valid index in a list (or the highest valid index, if 0 was supplied)

(check-expect (prev-index L-1 1) 0)
(check-expect (prev-index L-2 2) 1)
(check-expect (prev-index L-2 15) 4)
(check-expect (prev-index L-3 3) 2)
(check-expect (prev-index L-4 0) 1)
(check-expect (prev-index L-4 2) 1)
(check-expect (prev-index L-4 1) 0)
(check-expect (prev-index '() 1) 0)

(define (prev-index list i)
  (cond [(empty? list) 0]
        [(cons? list) (if (invalid-index? list i)
                          (sub1 (length list))
                          (sub1 i))]))

; invalid-index? : (X) Nat [List-of X] -> Boolean
; Return true if the given index is 0 or greater than the number of indices in the list;
; false otherwise

(check-expect (invalid-index? L-1 5) #t)
(check-expect (invalid-index? L-1 2) #f)
(check-expect (invalid-index? L-2 2) #f)
(check-expect (invalid-index? L-2 15) #t)

(define (invalid-index? list i)
  (or (zero? i) (> i (sub1 (length list)))))

; return-former : Any Any -> Any
; Returns the first argument
(check-expect (return-former 1 2) 1)
(check-expect (return-former "1" "7") "1")
(check-expect (return-former (list 1 4 3) "hello") (list 1 4 3))

(define (return-former arg1 arg2)
  arg1)


; every function and data definition up to this point, starting with training-fnames, was copied from
; Cameron Boggio's previous homeworks. The data definitions for Training, Testing, Instance, and
; Neighbor, however, were taken from the sample solutions from Piazza.


; euclidean-dist : String String -> Number
; Determines the euclidean distance between the Features of each file

(check-within
 (euclidean-dist "eucl-test-files/test1.txt" "eucl-test-files/test2.txt")
 (sqrt 193)
 0.01)
(check-within
 (euclidean-dist "eucl-test-files/test3.txt" "eucl-test-files/test4.txt")
 (sqrt 92)
 0.01)
(check-within
 (euclidean-dist "eucl-test-files/test5.txt" "eucl-test-files/test6.txt")
 (sqrt 21)
 0.01)

(define (euclidean-dist s1 s2)
  (local [(define LOF-S1 (flatten (read-lolon s1)))
          (define LOF-S2 (flatten (read-lolon s2)))
          (define (eucl-calculation num1 num2)
            (sqr (- num2 num1)))]
    (sqrt (foldr + 0 (map-2list LOF-S1 LOF-S2 eucl-calculation)))))

; nearest-neighbor : [List-of Neighbor] -> Neighbor
; Returns the neighboring feature with the smallest euclidean distance
(check-expect (nearest-neighbor (list NEIGHBOR-1 NEIGHBOR-2 NEIGHBOR-4 NEIGHBOR-5))
              NEIGHBOR-1)
(check-expect (nearest-neighbor (list NEIGHBOR-5 NEIGHBOR-2 NEIGHBOR-4))
              NEIGHBOR-2)
(check-expect (nearest-neighbor (list NEIGHBOR-5 NEIGHBOR-ZERO NEIGHBOR-4))
              NEIGHBOR-ZERO)

(define (nearest-neighbor lon)
  (smallest-of-list-by-f neighbor-dist lon))

; file->image : String -> Image
; From the given file return its corresponding image
(check-expect (file->image "numbers2.txt") (bitmap->image (list (list 21 32 81 79)
                                                                (list 100 100 100 100))))
(check-expect (file->image "numbers3.txt") (bitmap->image (list (list 43 15 26 134)
                                                                (list 94 17 139 19)
                                                                (list 169 171 4 18)
                                                                (list 17 187 183 4))))
(define (file->image s)
  (bitmap->image (read-lolon s)))
