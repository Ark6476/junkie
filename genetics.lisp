(load "utils.lisp")
;;;;;;;;;;;;;

(defparameter *encoding-table* (make-hash-table-with-mappings (:test 'equal)
                          ("0000" 0)
                          ("0001" 1)
                          ("0010" 2)
                          ("0011" 3)
                          ("0100" 4)
                          ("0101" 5)
                          ("0110" 6)
                          ("0111" 7)
                          ("1000" 8)
                          ("1001" 9)
                          ("1010" +)
                          ("1011" -)
                          ("1100" *)
                          ("1101" /))
  "Encoding Table. A four digit binary string represents either a number or mathematical operator")
(defparameter *word-length* 4 "size of one instruction")
(defparameter *chromosome-length* 7 "size of one chromosome (number of words)")

(defparameter *population-size* 10 "size of any generation")

(defparameter *current-generation* nil "List of current chromosomes")
(defparameter *current-generation-number* 0 "Counter to see how many generations it takes to find the answer")

(defparameter *crossover-rate* (/ 7 10)
  "The chance that two chromosomes will swap their bits")

(defparameter *mutation-rate* (/ 1 1000)
  "The chance that a bit within a chromosome will be flipped")

(defparameter *target-number* 42 "The number we're trying to find")

;;;;;;;;;;;;; Chromosome functions

(defun make-random-chromosome (&rest doesnothing)
  (with-output-to-string (stream)
    (loop repeat (* *word-length* *chromosome-length*) do
         (write-string (format nil "~a" (random 2)) stream))))

(defun decode-chromosome-raw (chromosome)
  "Decodes every word of a chromosome. Including junk values."
  (let ((decoded-chromosome))
    (loop for index from 0 to (- *chromosome-length* 1)
       do
         (setf decoded-chromosome
               (append decoded-chromosome (list (decode-word (get-word chromosome index))))))
    decoded-chromosome))

;TODO: Handle divide by zero bug
(defun decode-chromosome (chromosome)
  "Returns a valid list in the form of (number (operator number)*)"
  (let ((decoded-chromosome) (need-number T))
    (loop for decoded-word in (decode-chromosome-raw chromosome)
       do
         (when (and (not (null decoded-word))
                    (or (and need-number (numberp decoded-word))
                        (and (not need-number) (not (numberp decoded-word)))))
           (setf decoded-chromosome (append decoded-chromosome (list decoded-word)))
           (setf need-number (not need-number))))
    (when (not (numberp (first (last decoded-chromosome)))) ;Trim off trailing operators
      (setf decoded-chromosome (subseq decoded-chromosome 0 (- (length decoded-chromosome) 1))))
    decoded-chromosome))

(defun eval-decoded-chromosome (decoded-chromosome)
  "Evals an expression with the rightmost operators having priority"
  (let ((num-words (length decoded-chromosome)))
    (cond ((= num-words 1) (first decoded-chromosome))
          (T (eval (list (second decoded-chromosome)
                         (first decoded-chromosome)
                         (eval-decoded-chromosome (subseq decoded-chromosome 2))))))))
  
(defun eval-chromosome (chromosome)
  (eval-decoded-chromosome (decode-chromosome chromosome)))

(defun get-word (chromosome n)
  "Returns the Nth word of the chromosome"
  (subseq
   chromosome
   (* n *word-length*)
   (+ (* n *word-length*) *word-length*)))

(defun decode-word (word)
  (gethash word *encoding-table*))

(defun fitness-score (chromosome)
  "Hitting the target will trigger a divide by zero error"
  (/ 1 (abs (- *target-number* (eval-chromosome chromosome)))))

;;;;;;;;;;;;;;;;; Population functions

(defun probability (a-fraction)
  "Pass this the fraction 7/10 and seven out of ten times it will return true"
  (let ((full-range (denominator a-fraction))
        (true-range (numerator a-fraction)))
    (if (< (random full-range) true-range)
        T
        nil)))
    

(defun make-initial-population ()
  (mapcar #'make-random-chromosome (make-list *population-size*)))

(defun flip (bit)
  "Flip a bit!"
  (cond ((= 1 bit) 0)
        ((= 0 bit) 1)
        (t (error "You suck at flipping bits."))))

(defun mutate-bit (bit &optional (mutation-rate *mutation-rate*))
  "Flips the bit depending on the mutation-rate"
  (if (probability mutation-rate) (flip bit) bit))

(defun convert-1-or-0-to-char (one-or-zero)
  (cond ((equal one-or-zero 0) #\0)
        ((equal one-or-zero 1) #\1)
        (t (error "Nope"))))

(defun mutate (chromosome &optional (mutation-rate *mutation-rate*))
  "Mutate a chromosome depending on the mutation-rate"
  (map 'string #'(lambda (c)
                   (CONVERT-1-OR-0-TO-CHAR
                    (mutate-bit (digit-char-p c) mutation-rate)))
       chromosome))

(defun do-crossover (chromosome1 chromosome2)
  "Select a random position in chromosome1 and do the crossover"
  (let ((crossover-index (random (length chromosome1))))
    (concatenate 'string
                 (copy-seq (subseq chromosome1 0 crossover-index))
                 (copy-seq (subseq chromosome2 crossover-index)))))
  
(defun crossover (chromosome1 chromosome2 &optional (crossover-rate *crossover-rate*))
  "Crossover the two chromosomes if the crossover-rate happens. Otherwise return a copy of chromosome1"
  (if (probability crossover-rate)
      (do-crossover chromosome1 chromosome2)
      (copy-seq chromosome1)))

(defun breed (chromosome1 chromosome2)
  (mutate (crossover chromosome1 chromosome2)))

(defun get-next-generation (generation)
  (let ((next-generation nil) (generation (sort-chromosomes generation)))
    (dotimes (i (length generation))
      (setf next-generation (append next-generation (list (breed
                                                           (select-chromosome-for-breeding generation)
                                                           (select-chromosome-for-breeding generation))))))
    next-generation))
      
(defun sort-chromosomes (generation)
  (sort (copy-seq generation) #'< :key #'fitness-score))

(defun select-chromosome-for-breeding (generation)
  "Use roulette wheel selction to pick a chromosome (proportional to fitness score)"
  (let ((random-fitness-score (random (float (apply '+ (mapcar #'fitness-score generation)))))
        (sum 0))
    (loop for chromosome in generation
       do
         (setf sum (+ sum (float (fitness-score chromosome))))
         (when (> sum random-fitness-score)
           (return chromosome)))))


;;;;;;;;;;;;;

(defparameter MYCHROMO (make-random-chromosome))
(defparameter MYPOP (make-initial-population))
