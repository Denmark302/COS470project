(defvar object)    						 ;hold the list of the coord of objects, agents ,and the size of the world

(defun start()
(setf test-world (make-instance 'world)) ;world is stored as alist of 2 element list as (x, y)s
(input)            						 ;this automaticly read in the file
(read_input object)						 ;this passes the object into a gobal variable

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; reads input from a file into a global variable 
;;; as a list in a list.
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun input()

	(with-open-file(in #P "testInput.txt" :direction :input)
			(loop for line = (read in nil 'A)
	 until (eq line 'A)
	 do (setq object line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Loop though the list in a nested loop
;;; Then look though the list for specfic variables.
;;; Once these if has found the variables it is then passed 
;;; the number value into a node.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read_input(x)
	(loop for a in x 
		do(loop for p in a
			do (cond
				 ((eq 'G p);not working currelnty because we need a search for the pathing to work.
				 	(      ;This set a variable goal in a list (x y z) z for height
				 		setf goal(list (nth 1 a)(nth 2 a)( nth 3 a)))
				 		;(setf path ("search" test-world goal))
				 	)
				 ((eq 'O p);this should pass in any obstacles that are in the list an set it in the test world
				 	(setf (obstacle test-world) (append (obstacle test-world) (list(list (nth 1 a) (nth 2 a))))))
				 ((eq 'M p);this creats the size of the map
				 	(setf (w-nodes test-world) (append (w-nodes test-world) (list(list (nth 1 a)  (nth 2 a))))))
				 ((eq 'R p) ;this creates an agent ROBOT (cmax)(alt)(pos (x y))
				 	(create-agent (nth 1 a) (nth 2 a) (nth 3 a)))))))




;------------------------------------------------------------------------------------------------------------------------------------


(defclass wo-node () ;represents a node in the world, will be used in a list to represent the world
  ((xpos :accessor x-pos  ;x-position
	 :initform 1
	 :initarg :xpos)
   (ypos :accessor y-pos ;y-position
	 :initform 1
	 :initarg :ypos)
   (zpos :accessor z-pos ;z-position or height above sea level, 0 is sea-level
	 :initform 0
	 :initarg :zpos)
   (parent :accessor par ;note, this is used when the pathfinding functionality is required
	   :initform nil
	   :initarg :parent)))

(defun create-node(x y z par) ;creates a world node with x y z coordinates, note leave par nil if you are representing the world
  (make-instance 'wo-node :xpos x :ypos y :zpos z :parent par))

(defun tox_y(w-node)          ;returns an x-y coorinate pair for a wo-node
  (list (x-pos w-node) (y-pos w-node)))

(defun tox_y_z(w-node)        ;returns an x-y-z coordinate list for a wo-node
  (list (x-pos w-node) (y-pos w-node) (z-pos w-node)))

(defun inset-world(test world)      ;determines if a x-y pair exists in our world
  (loop for x from 0 to (-(list-length world)1) do
       (if (= (x-pos(nth x(w-nodes world))) (nth 0 test))
	   (if (= (y-pos(nth x(w-nodes world))) (nth 1 test))
	       (return 1)))))

(defclass world()                            ;world object holds nodes and inaccessable locations
  ((nodes :accessor w-nodes                  ;world locations
	  :initform (list )
	  :initarg :nodes)
   (obs :accessor obstacle                   ;obstacle locations
	:initform (list )
	:initarg :obs)
   (agent-pos :accessor a-pos               ;position of agent (x y coordinates)
	      :initform nil
	      :initarg :agent-pos)
   (agent-dir :accessor a-dir               ;direction of agent
	      :initform 'north
	      :initarg :agent-dir)))


(defclass agent()                           ;agent representation
  ((climb-max :accessor c-max               ;maximum climbing height
	      :initform 0
	      :initarg :climb-max)
   (compass :accessor comp                  ;internal compass direction
	    :initform 'north
	    :initarg :compass)
   (altitude :accessor alt                  ;internal altitude measure
	     :initform 0
	     :initarg :altitude)
   (knowledge :accessor know                ;machine's knowledge (in wo-node form) machine will only act on this knowledge
	      :initform (list )
	      :initarg :knowledge)
   (GPS-location :accessor loc
		 :initform (list )
		 :initarg :GPS-location)))
(defun create-agent(cmax alt loca)          ;create an agent at a posiion and altitude, make sure altitude is correct when placing agent, location input is in a (list x y) form
  (make-instance 'agent :climb-max cmax :altitude alt :GPS-location loca))

  
