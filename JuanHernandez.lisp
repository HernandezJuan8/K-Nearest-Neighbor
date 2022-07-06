;;;;; K-NEAREST-NEIGHBOR
;;;;; Juan Hernandez jherna56
;;;;; Project 1
;;;;; G01234626 
;;;;; CS 480 Section 001 

;;;;; This is a very short assignment and its intent is to get your feet wet in Lisp.
;;;;; Your task is to write three functions which will enable you to test K-Nearest Neighbor:
;;;;;
;;;;; 1. DIFFERENCE. This function tells you the difference (error) between two lists.
;;;;; You will use three kinds of measurement for error: COUNT, SQUARED, and MANHATTAN.
;;;;; Though the default will be COUNT, you will probably be most familiar with SQUARED
;;;;; since it basically does what the lecture notes showed.
;;;;;
;;;;; 2. K-NEAREST-NEIGHBOR. This function will take a list of examples, and their associated
;;;;; classes, and return what class they predict for an additional unseen example, based on
;;;;; the classes of the K examples whose DIFFERENCE from the unseen example is lowest.
;;;;;
;;;;; 3. GENERALIZATION. This function will take two lists of examples, a training set and a
;;;;; testing set, and return the percentage of testing set examples which were correctly
;;;;; predicted by K-Nearest-Neighbor using the original training set examples.
;;;;;
;;;;; You must do the following:
;;;;;
;;;;; 1. Write the three functions correctly.
;;;;; 2. Test them on the two EXAMPLES below. Determine the correct values.
;;;;; 3. Modify this file into properly-commented and properly compiling lisp code,
;;;;; which has at the end of it, in comments, the answers to the two EXAMPLES questions,
;;;;; PLUS an at least 500-word report detailing how you went about implementing this code
;;;;; and what you learned along the way.
;;;;;
;;;;; You should submit this file as a single LISP file named FooBar.lisp, where Foo is your
;;;;; first name and Bar is your last name (for example, my file would be named SeanLuke.lisp).
;;;;;
;;;;; This file will be sent to your TA. We do not have instructions
;;;;; yet as to how to do this, but we will soon, stay tuned.
;;;;;
;;;;; You should verify your code on 'lisp' on mason.gmu.edu. Here's how it works. If the TAs
;;;;; cannot get your code running on their laptops, and have reduced your score because of it,
;;;;; you have the right to challenge this if you believe that the program works on mason.gmu.edu.
;;;;; If it does, you're fine. Don't develop on mason.gmu.edu, that's painful. Just verify the
;;;;; final result on it before submission.
;;;;;
;;;;; Please do not deviate from the function signatures and data format provided. Though you may
;;;;; have a more clever way of doing things (such as using arrays rather than lists) all you are
;;;;; really doing is making life far harder for the TAs to grade. Thus deviation of any kind,
;;;;; even notional improvements, will considered a CODE ERROR.





(defun difference (list1 list2 &key (measure 'count))
"Returns the DIFFERENCE (or ERROR) between two lists of things, using one of three measures:

1. Measure type COUNT: the number of times an item in list1 is different from
an item in list2 at the same position. The items don't have to be numbers.
Example usage: (difference '(red blue bob) '(blue blue george) :measure 'count) -> 2

2. Measure type SQUARED: the sum squared difference between items in list1 and list2
Obviously, the items must be numbers of some sort.
Example usage: (difference '(1 2 3) '(1 3 7) :measure 'squared) -> 17

2. Measure type MANHATTAN: the sum absolute difference (abs (- a b)) between items
in list1 and list2. Obviously the items must be numbers of some sort.
Example usage: (difference '(1 2 3) '(1 3 7) :measure 'manhattan) -> 5"

;;; HINTS: use equalp to test for the given type, and for equality in COUNT
;;; HINTS: ABS is absolute value
;;; HINTS: I used MAPCAR and LAMBDA to make things easy. You might look them up.
;;; HINTS: This is a place where INCF would be reasonable.
;;; HINTS: My code is about 14 (short) lines long
;;;; IMPLEMENT ME
(cond  ((equalp measure 'count) ; checks if its count
	(abs (- (length list1) (count T (mapcar (lambda (j k) (equalp j k)) list1 list2)))))
       ; this will return how many times an item is different by counting how many is different in a list.
       ((equalp measure 'squared) ; checks if its squared
	(reduce '+ (mapcar (lambda (j k) (* (- j k) (- j k))) list1 list2))) ; returns the sum squared difference 
       ((equalp measure 'manhattan) ; checks if its manhatten 
	(reduce '+ (mapcar (lambda (j k) (abs (- j k))) list1 list2))))) ;; checks for sum of abs val difference



;;; I am providing this function for you as an example
(defun most-common (list)
"Returns the most common item in list, breaking ties arbitrarily"
(let ((reduced-elts (mapcar (lambda (elt) (list elt (count elt list)))
(remove-duplicates list))))
(first (first (sort reduced-elts #'> :key #'second)))))


(defun k-nearest-neighbor (examples new-example &key (k 1) (measure 'count))
"Given a LIST of EXAMPLES, each of the form (list-of-data class), and a NEW-EXAMPLE
of the same form, determines the K EXAMPLES which most closely resemble the NEW-EXAMPLE,
then returns the most common class among them. The MEASURE used to compute similarity
can be provided. Note that the CLASS is a LIST, like (0.9), not 0.9."

;;; HINTS: use MOST-COMMON to select the most common among N classes.
;;; HINTS: you might use SORT and SUBSEQ
;;; HINTS: I used MAPCAR and LAMBDA to make things easy. You might look them up.
;;; HINTS: My code is about 11 lines long
;;;; IMPLEMENT ME
(let ((sorted (sort (copy-list examples) 
		    (lambda (e f) (< (difference (elt e 0) (elt new-example 0) :measure measure) 
				     (difference (elt f 0) (elt new-example 0) :measure measure))))))
  ; the above sorts the list by seeing if the difference is less than next difference and storing in a variable
  (let ((voters (subseq sorted 0 k))) (most-common (mapcar (lambda (e)(elt e 1)) voters)))))
; grabs 0 to k lists then grabs the most common label and returns it

(defun generalization (training-examples test-examples &key (k 1) (measure 'count))
"Given a list of TRAINING EXAMPLES, each of the form (list-of-data class),
and a set of TEST EXAMPLES, each of the same form, returns the percentage of TEST
EXAMPLES correctly classified when passed to K-NEAREST-NEIGHBOR with the TRAINING
EXAMPLES. Note that the CLASS is a LIST, like (0.9), not 0.9."

;;; HINTS: use EQUALP to compare classes
;;; HINTS: This is a place where INCF would be reasonable.
;;; HINTS: I used DOLIST
;;; HINTS: My code is about 5 lines long

;;;; IMPLEMENT ME
(let ((train (mapcar
	      (lambda (e)(k-nearest-neighbor training-examples e :k k :measure measure)) training-examples)))
  ;above stores in a list labels of the training example from knn
  (let ((test (mapcar (lambda (f) (k-nearest-neighbor training-examples f :k k :measure measure)) test-examples)))
    ; above stores in a list labels of test example from knn
    (let ((predict (mapcar (lambda (j k) (equalp j k)) train test)))
      ; above then gets a list to see which ones are right and which ones arent
      (float (/ (count T predict) (length predict))))))) ; counts the number of T in the list and then divide by
; length and return it as a float 




;;;;; SOME EXAMPLES
;;;;;
;;;;; Find out what *VOTING-RECORDS-SHORT* would predict about the *NEUTRAL* congressman using 3-Nearest Neighbor
;;;;; and sum-squared error distance.
;;;;;
;;;;; (k-nearest-neighbor *voting-records-short* *neutral* :k 3 :measure 'squared)
;;;;;
;;;;; Find out how well the first 200 voting records predict the last 94 voting records using 3-Nearest Neighbor
;;;;; and sum-squared error distance.
;;;;;
;;;;; (generalization (butlast *voting-records-short* 94) (last *voting-records-short* 94) :measure 'squared :k 3)
;;;;;
;;;;; WHAT ARE THE CORRECT ANSWERS TO THESE TWO EXAMPLES? 


(defparameter *neutral*
'((0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5) (0.1))) ;; is this the right class?


(defparameter *voting-records-short*
'(((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.5 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.9 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.5 0.1 0.9 0.9 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.9 0.5 0.9 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.9 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.9 0.9 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.5 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5 0.9 0.9 0.5 0.5) (0.1))
((0.9 0.9 0.9 0.5 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.5 0.9) (0.9))
((0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1) (0.9))
((0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9) (0.1))
((0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.1 0.9 0.9 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.5 0.5 0.5 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.5 0.9 0.9 0.5 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.5 0.9 0.9 0.9 0.1 0.9 0.5 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.5 0.9) (0.9))
((0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.5 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.5 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.5 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.5 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.9 0.1 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.5 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.5 0.9 0.1 0.1) (0.9))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.5 0.1) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.5 0.5 0.5 0.9 0.1 0.5) (0.1))
((0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.9 0.5 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.5 0.9 0.5 0.5) (0.1))
((0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.5 0.1 0.5) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5 0.9 0.9 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.5 0.5 0.1 0.9 0.5 0.5 0.5 0.9 0.9) (0.9))
((0.5 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.5 0.5 0.5 0.5 0.5 0.5) (0.9))
((0.5 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.9 0.5 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.9 0.5 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.5 0.9 0.9 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.5 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.5 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.5 0.9 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.5 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.9 0.1 0.9 0.9 0.9 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.5 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.5 0.9 0.9 0.1 0.1) (0.9))
((0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.5 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.5 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.5 0.9 0.5 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.5 0.1 0.9 0.5) (0.9))
((0.1 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.9 0.1) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.9 0.1) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.5 0.9 0.1 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.5 0.5 0.5 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.5 0.5) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5 0.5 0.1 0.5) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.5 0.5 0.9 0.9) (0.9))
((0.9 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.5 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.5) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.5 0.9 0.9 0.1 0.5) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.5 0.9 0.9) (0.9))
((0.1 0.1 0.5 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.5 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.5 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.5 0.9 0.1 0.1 0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.1 0.5 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5) (0.9))
((0.5 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.5 0.5 0.1 0.1 0.1 0.5 0.5) (0.9))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.1 0.5 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.5) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.5 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.5 0.1 0.1 0.1 0.1 0.5 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.5 0.9 0.9 0.9 0.1 0.5 0.5 0.1 0.5 0.5 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.5 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.9 0.5 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.5 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.5 0.5 0.5 0.5 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.5) (0.1))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.5 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.5 0.9 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.5 0.9 0.9 0.5 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.1) (0.9))
((0.1 0.9 0.9 0.1 0.5 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.5 0.5 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.9 0.5 0.9 0.9 0.9 0.5 0.1 0.1 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.5 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.5 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.5 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.1 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.5 0.5) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.9 0.5 0.9 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.5 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.9 0.9 0.9 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.5 0.1 0.1 0.1 0.1 0.9 0.5 0.1) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.5 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.5 0.9 0.1 0.5 0.5 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.5 0.9 0.9 0.5 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.5 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.5 0.1 0.9 0.9 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.1 0.1) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.5 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.5 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.1 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.5 0.1 0.9 0.9 0.1 0.1) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.1 0.5) (0.1))
((0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.5 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5 0.1 0.9 0.1 0.9) (0.9))
((0.1 0.1 0.5 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.5) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.5 0.9 0.1 0.5 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.1) (0.1))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1) (0.9))
((0.1 0.5 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.5 0.5 0.1 0.1 0.5 0.9 0.5 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.1) (0.9))
((0.9 0.9 0.5 0.5 0.5 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.5 0.1 0.1 0.1 0.9 0.1 0.1 0.9 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.1 0.1 0.9 0.5 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))))

;;;;; Answer to question 1: (0.9)
;;;;; Answer to question 2: .5638298

;;;;; essay
;;;;; I implemented difference by first using cond to check for what measure the function is supposed to do
;;;;; when it got count I first use mapcar and lambda to check if two items from both lists in the same position
;;;;; are the same, that'll put it into a list then I use count to count the numeber of T there are in the list
;;;;; and subtract that from the length of the list (I use list1 here since it would be the same length)
;;;;; and return that if it is count. For Squared, I use a mapcar and lambda to check two items in the same
;;;;; in the same position to to subtract it then square and put it all into a list. I then use reduce to add
;;;;; to add everything up in the list and return that as the sum squared difference. In manhatten it is almost
;;;;; similar that I use mapcar and lambda to subtract it but instead of squared I just get the absolute value
;;;;; and gets put into an array then use reduce to add up everything and return that.
;;;;; For KNN function, I first use let to create a local variable and use sort and set the list I use as
;;;;; a copy of that list. the predicate that is used to sort is seeing the difference at the current position item
;;;;; and the new-example which I am using to sort it from closest-to farthest. I then create another local
;;;;; variable and set it as voters being the subseq from 0 to K to only use that list, then I use mapcar and
;;;;; lambda to create a list that uses contains the labels of the voter and then use most-common function
;;;;; to return, the most common label.
;;;;; The generalization function starts with a local variable I called train which will contain the labels of
;;;;; all the training data by using knn and then another local variable which is called test which will
;;;;; contain all the labels of the test examples made using knn with test examples. I then create another local
;;;;; variable that will contain a list of nils and T to see if test example was correctly classified with the
;;;;; the training examples and this is done by checking if it's equal to each other to give T or nil.
;;;;; then it will take how many it got correct and divide that by the length of the test examples and return
;;;;; as a float.
;;;;; What I learned along the way is how powerful mapcar and lambda are to quickly do operations. I really liked
;;;;; using them as it removed a bunch of code that I wouldve had to use in if this was in C or java or python.
;;;;; I also learned that it can be really simple to just do something and return it and that it you dont have to
;;;;; do as much as you have into as in the other languages. Another thing I learned was that you want to do
;;;;; to do code in lisp and an ai that wont be too costly and take up too much resource or time.
;;;;; I hope that my answers are correct as It seems correct after trying the function and making sure it
;;;;; was doing what I wanted it to do first having to do pseudo code on paper to make sure
;;;;; I understand what the algorithm was going to be.
