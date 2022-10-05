(defun hanoi-1(n from to spare)  ; The hanoi-1 function n, from, to, spare are all arguments. 
  (cond
    ((> n 0)                     ; RERCURSIVE move a tower of more than zero disks.
     (hanoi-1 (- n 1) from spare to)                 ; Move a 1-disk-smaller tower where you are NOT going.
     (format t "Move disk from ~s ==> ~s~&" from to) ; Move the bottom disk where you ARE going.
     (hanoi-1 (- n 1) spare to from)                 ; Move the smaller tower back on top of it.
     )
    )                            ; The BASE case is implied (n = 0) here, where you do nothing.
  )

(defun hanoi (n)                 ; General hanoi fuction
  (format t "~%Moving ~s disk~p from tower A to tower C:~%" n n) ; ~p forms plural s
  (hanoi-1 n 'A 'C 'B)
  )

(format t "Liliana Hotsko. Hanoi Towers.")
(format t "to run enter (hanoi n), where n is the number of disks")

#|
CL-USER(66): (hanoi 1)

Moving 1 disk from tower A to tower C:
Move disk from A ==> C
NIL

CL-USER(67): (hanoi 2)

Moving 2 disks from tower A to tower C:
Move disk from A ==> B
Move disk from A ==> C
Move disk from B ==> C
NIL

CL-USER(68): (hanoi 3)

Moving 3 disks from tower A to tower C:
Move disk from A ==> C
Move disk from A ==> B
Move disk from C ==> B
Move disk from A ==> C
Move disk from B ==> A
Move disk from B ==> C
Move disk from A ==> C
NIL

CL-USER(69): (hanoi 4)

Moving 4 disks from tower A to tower C:
Move disk from A ==> B
Move disk from A ==> C
Move disk from B ==> C
Move disk from A ==> B
Move disk from C ==> A
Move disk from C ==> B
Move disk from A ==> B
Move disk from A ==> C
Move disk from B ==> C
Move disk from B ==> A
Move disk from C ==> A
Move disk from B ==> C
Move disk from A ==> B
Move disk from A ==> C
Move disk from B ==> C
NIL
CL-USER(70): 

|#
