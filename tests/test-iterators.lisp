(in-package #:containers)

(collect-elements 
 (make-iterator '(1 2 3 4 5) :filter #'oddp))

;;; ---------------------------------------------------------------------------

(lift:deftestsuite test-iterators ()
  ())

(lift:addtest (test-iterators)
  collecting-nil
  (lift:ensure-same nil (collect-elements (make-iterator nil :filter #'oddp))))

(lift:addtest (test-iterators)
  collecting-one-even
  (lift:ensure-same nil (collect-elements (make-iterator '(2) :filter #'oddp))))

(lift:addtest (test-iterators)
  collecting-odd-even 
  (lift:ensure-same '(3) (collect-elements (make-iterator '(3 2) :filter #'oddp))))

(lift:addtest (test-iterators)
  collecting-even-odd
  (lift:ensure-same '(3) (collect-elements (make-iterator '(2 3) :filter #'oddp))))

(lift:addtest (test-iterators)
  collecting-list 
  (lift:ensure-same '(3 1 3) 
                    (collect-elements (make-iterator '(3 2 4 1 3 2) :filter #'oddp))))

(lift:addtest (test-iterators)
  collecting-list 
  (lift:ensure-same '(3 1) 
                    (collect-elements (make-iterator '(3 2 4 1 3 2)
                                                     :filter #'oddp
                                                     :unique t))))



(lift:deftestsuite test-iterators ()
  ())

(lift:addtest (test-iterators)
  collecting-nil
  (lift:ensure-same nil (collect-elements (make-iterator #() :filter #'oddp))))

(lift:addtest (test-iterators)
  collecting-one-even
  (lift:ensure-same nil (collect-elements (make-iterator #(2) :filter #'oddp))))

(lift:addtest (test-iterators)
  collecting-odd-even 
  (lift:ensure-same '(3) (collect-elements (make-iterator #(3 2) :filter #'oddp))))

(lift:addtest (test-iterators)
  collecting-even-odd
  (lift:ensure-same '(3) (collect-elements (make-iterator #(2 3) :filter #'oddp))))

(lift:addtest (test-iterators)
  collecting-list 
  (lift:ensure-same '(3 1) 
                    (collect-elements (make-iterator #(3 2 4 1 3 2) 
                                                     :filter #'oddp
                                                     :unique t))))
#|


(setf i (make-iterator '( 4 1 3 5) :filter #'evenp))
(current-element i)
(move-forward-p i)
(move-forward i)
(current-element-p i)

(collect-elements (make-iterator #2A((3 2 4) (1 3 2)) 
                                 :filter #'oddp
                                 :unique t
                                 :transform #'u::square))

(collect-elements 
 (make-iterator #(3 2 4 1 3 2) 
                :filter #'oddp
                :unique t
                :transform #'u::square))
|#

#|
(bind ((c (make-container 'list-container :initial-contents '(1 2 3)))
       (i (make-iterator c)))
  (print (next-element i))
  (print (next-element i))
  (print (next-element i))
  (reset i)
  (print (next-element i))
  (print (next-element i))
  (print (next-element i))
  (values))
|#