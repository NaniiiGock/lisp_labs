(defun animal () 
  (defnode 'thing "Is this thing a mammal" 'cow 'lizard)
  (loop
   (run-node 'thing)
    (when (not (ask-play-again)) 
      (format t "Thanks for playing. [Use (saveit \"fname.lisp\") if you want to save your data.]~%")
      (return))))

(setq *normal-win-responses*
      (list "I thought so"
	    "Hah, you cannot fool me!"
            "Hurray for me!"))
(setq *normal-lose-responses*
      (list "Ahh -- nuts."
       "Rats, I thought I knew that one"
       "Fooey, I will get you next time"))

(setq *depressed-win-responses1*
      (list "I am usually wrong, but got that right."
        "For once I guess I am right."
        "I am sorry I am the winner."))
(setq *depressed-lose-responses1*
      (list "Yes I expected to lose."
        "I always do lose. It is normal for me."
        "I am glad I am the loser. I deserve it."))
; (setq *depressed-win-responses2*
;       (list "")
; )
; (setq *depressed-lose-responses2*
;       (list "")
; )
; (setq *depressed-win-responses3*
;       (list "")
; )
; (setq *depressed-lose-responses3*
;       (list "")
; )


(setq *manic-win-responses1*
      (list "hahhaaaaa, I am genious!!!"))
(setq *manic-lose-responses1*
      (list "that is the only time I lose, I am the champion! Almost)"))
; (setq *manic-win-responses2*
;       (list "")
; )
; (setq *manic-lose-responses2*
;       (list "")
; )
; (setq *manic-win-responses3*
;       (list "")
; )
; (setq *manic-lose-responses3*
;       (list "")
; )

(setq *win-responses*
      (list "hahhaaaaa, I am genious!!!"))
(setq *lose-responses*
      (list "that is the only time I lose, I am the champion! Almost)"))


(defun gloat (intensity)
  (let ((i (random (length *win-responses*))))
    (format t "~A~%" (nth i *win-responses*)))
    (setf intensity (+ intensity 1))
    (make-personality intensity))

(defun i-lost (intensity)
  (let ((i (random (length *lose-responses*))))
    (format t "~A~%" (nth i *lose-responses*)))
    (setf intensity (- intensity 1))
    (make-personality intensity))




(defun make-normal-personality ()
  (setq *win-responses* *normal-win-responses*)
  (setq *lose-responses* *normal-lose-responses*)
)

(defun make-depressed-personality (intensity)
  (if (> intensity -2) 
  ((setq *win-responses* *depressed-win-responses1*)
  (setq *lose-responses* *depressed-lose-responses1*))
  (if (< intensity -3) 
  ((setq *win-responses* *depressed-win-responses3*)
  (setq *lose-responses* *depressed-lose-responses3*))
  ((setq *win-responses* *depressed-win-responses2*)
  (setq *lose-responses* *depressed-lose-responses2*))
  ))
)

(defun make-manic-personality (intensity)
  (if (< intensity 2) 
  ((setq *win-responses* *manic-win-responses1*)
  (setq *lose-responses* *manic-lose-responses1*))
  (if (> intensity 3) 
  ((setq *win-responses* *manic-win-responses3*)
  (setq *lose-responses* *manic-lose-responses3*))
  ((setq *win-responses* *manic-win-responses2*)
  (setq *lose-responses* *manic-lose-responses2*))
  ))
)

(defun make-personality (intensity)
  (format t "Intensity ~s ~%" intensity)
  (if (> intensity 0) (make-manic-personality intensity)
  (if (< intensity 0) (make-depressed-personality intensity) (make-normal-personality)))
  )

(setf intensity 0)
(make-personality intensity)
;;; =============================================================
(defun saveit (filename)                       ; defining saver
  (if (stringp filename)                       ; check if right format of filname (string)
      (let ((saver (open filename :direction :output 
			 :if-exists :supersede)))                ; if open existing file - rewrite, else create new
	(format saver "(setq *nodes* ~%'")           ; show info to user
	(pprint *nodes* saver)                       ; Save our database of animals to a file.
	(format saver ")~%(setq *node-count* ~s)" *node-count*) ; writing to the file
        (close saver))                         ; necessary closure
      (format t "Sorry, filename must be a string~%")))


;general functions for setting the new value to the node number
(defvar *nodes*) (setq *nodes* nil)
(defvar *node-count*) (setq *node-count* 1)
(defun node-count () (incf *node-count*))

; helping functions for creating new branches from the node
(defun node-name (n)       (first n))
(defun node-question (n)   (second n))
(defun node-yes-branch (n) (third n))
(defun node-no-branch (n)  (fourth n))

;;;creating 2 branches from the current node, leafs - animals
(defun defnode (name question yes-branch no-branch)
  (setq *nodes*
	(cons (list name question yes-branch no-branch) *nodes*)))

; check if node among nodes
(defun get-node (name)  (assoc name *nodes*))


(defun run-node (name)                     ; recursive function for search in binary tree
  (let ((n (get-node name)) (response nil)); find the appropriate node

    (if (equal (ask (node-question n)) 'y) ; winning case
	    (if (symbolp (node-yes-branch n))    ; check type 
        (if (guess (node-yes-branch n))    ; losing 
          (gloat intensity)                ; losing
        ;else:
            (setf (rest n)
            (list (second n) (add-node (node-yes-branch n)) ; add branch to exsisting node
            (fourth n))))                                   ; hack around xlisp shortcomming.
      ;else:
        (run-node (first (node-yes-branch n))))             ; start recursively again
    ;else:
      (if (symbolp (node-no-branch n))                      ; the same part after loosing
        (if (guess (node-no-branch n))                      ; but with no-branch
          (gloat intensity)
          (setf (rest n)
          (list (second n) (third n)
          (add-node (node-no-branch n)))))
      ;else
          (run-node (first (node-no-branch n))))
  )))

;;; add nodes to the binary search tree on the right side
;;; depending on the question and animals
(defun add-node (answer-tried)
  (let ((new-thing nil) (new-node-name (node-count)) (new-node nil))
    (i-lost intensity)                              ;print losing phrase
    (format t "~%What was it [type one word]?~%")   ; ask for answer
    (setq new-thing (read))                         ; create new object from input
    (format t "Type a question that is true for ~s and false for ~s [in quotes]:~%~%" 
    new-thing answer-tried)                         ;creating new branch after trying/checking
    (defnode new-node-name (read) new-thing answer-tried)
    (list new-node-name)))

;;;interaction with answers and questions
(defun ask (question) 
  (format t "~a? [y/n]~%" question)
  (read))
(defun ask-play-again ()
  (format t "~%Would you like to play again? [y/n]~%")
  (equal (read) 'y))
(defun guess (name)
  (format t "Is it a ~s? [y/n]~%" name)
  (equal (read) 'y))


;;;inroduction to the game
(format t "~%This is Liliana Hotsko's Animal Learning program~%")
(format t "~%To read in previous data [in MyData.lisp], ")
(format t "type (load \"MyData.lisp\") at the LISP prompt.~%~%")
(format t "To play animal, just type (animal) at the LISP prompt~%~%") 
