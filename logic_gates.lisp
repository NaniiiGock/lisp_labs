; module for training the current neuron for appropriate logic gate


;general function that shows how to use the program
(defun help ()
  (format t "(new-rand) to get random values for your neuron~%")
  (format t "(notgate n) (gate n n) where n is 0/1 to see current logic.~%")
  (format t "(train4NOT) (train4AND) (train4OR) (train4NAND) (train4NOR)are the training functions.~%")
  (format t "(help) to replay this list.~%")
  )
(help)

; set threshold, w1, w2 and epsilon 
;(only epsilon will not change while creating the new neuron)
(setf threshold 2) (setf w1 0) (setf w2 0) 
(setf epsilon 0.25)

;setting new values to threshold, w1 and w2 randomly (between 0 and 2)
;shows the values to the user
(defun new-rand ()
  (setf threshold (random 3))
  (setf w1 (random 3))
  (setf w2 (random 3))
  (format t "T: ~s, W1: ~s W2: ~s~%" threshold w1 w2))

;the current values that the gate produce with current weights
;it will change after training the neuron for the new logic-gate
(defun gate (v1 v2)
  (let ((i1 (* v1 w1)) (i2 (* v2 w2)) (summ 0))
    (setf summ (+ i1 i2))
    (if (>= summ threshold) 1 0)))

;has to return the oposit value to the given if trained
(defun notgate (val)
  (let ((summ 0))
    (setf summ (* val w1))
    (if (>= summ threshold) 1 0)))

;special training algorythm function for the not-gate
; uses formulas that will make the return colser and closer to the needed
(defun train-notgate (v1 target)
  (let ((delta 0)(actual 0))
    (setf actual (notgate v1))
    (setf delta (- target actual))
    (cond
      ((equal delta 0) t)
      (t
       (setf threshold (+ threshold (* -1 epsilon delta)))
       (setf w1 (+ w1 (* epsilon delta v1)))))))

; general training function for every logic gate except of not gate
(defun train-gate (v1 v2 target)
  (let ((delta 0)(actual 0))
    (setf actual (gate v1 v2))
    (setf delta (- target actual))
    (cond
      ((equal delta 0) t)
      (t
       (setf threshold (+ threshold (* -1 epsilon delta)))
       (setf w1 (+ w1 (* epsilon delta v1)))
       (setf w2 (+ w2 (* epsilon delta v2)))))))


;trains the not gate to return the opposit value to the given
(defun train4NOT ()
  (dotimes (junk 20)
    (train-notgate 0 1)
    (train-notgate 1 0)
    (format t "T: ~s, W1: ~s~%" threshold w1)))


; functions that match to the gate-function-names that will be tested
; they call the train4gate function with appropriate values that
; have to be returned in each case in (0,0),(0,1),(1,0),(1,1)
(defun train4and ()
  (train4gate 0 0 0 1)
  )

(defun train4or ()
  (train4gate 0 1 1 1)
  )

(defun train4nor ()
  (train4gate 1 0 0 0)
  )

(defun train4nand ()
  (train4gate 1 1 1 0)
  )

; (defun train4xor(v1, v2)
;   (train4gate 0 1 1 0)
;   )

;general function with algorythm that maches to and, or, nor and nand
(defun train4gate (a b c d)
  (let ((rval nil))           ;set default return : NIL
    (dotimes (junk 30)        ;set the maximum times to run
      (let ((old-threshold threshold)(old-W1 w1)(old-w2 w2))     
      ; Get the current values to start
        (train-gate 0 0 a)
        (train-gate 0 1 b)
        (train-gate 1 0 c)
        (train-gate 1 1 d)
        (format t "T: ~s, ~cW1: ~s ~cW2: ~s~%" threshold #\tab w1 #\tab w2)
        ; show current result
        (when (and (equal old-threshold threshold) (equal old-w1 w1) (equal old-w2 w2)) 
        ; Training complete if it reaches the appropriate values 
          (setf rval t)       ; True that training is complete
          (return))))         ; Return from dotimes
    rval))


; (defun train4xor (a b c d)
;   (train4nand)
;   (let (a (gate ))



  (let ((rval nil))           ;set default return : NIL
    (dotimes (junk 30)        ;set the maximum times to run
      (let ((old-threshold threshold)(old-W1 w1)(old-w2 w2))     
      ; Get the current values to start
        (train-gate 0 0 a)
        (train-gate 0 1 b)
        (train-gate 1 0 c)
        (train-gate 1 1 d)
        (format t "T: ~s, ~cW1: ~s ~cW2: ~s~%" threshold #\tab w1 #\tab w2)
        ; show current result
        (when (and (equal old-threshold threshold) (equal old-w1 w1) (equal old-w2 w2)) 
        ; Training complete if it reaches the appropriate values 
          (setf rval t)       ; True that training is complete
          (return))))         ; Return from dotimes
    rval))


(new-rand) ;run the rand with setting new variables to threshold, w1 and w2 
