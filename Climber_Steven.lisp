;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;; 
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar object)                ;hold the list of the coord of objects, agents ,and the size of the world
(defvar worldMap '())          ;this hold a list of the world
(defvar worldSize '())           ;hold the size of the world in (x,y) form.

(defvar goal nil)


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

          (setf goal  (cdr a))

          )
         ((eq 'O p);this should pass in any obstacles that are in the list an set it in the test world
          ;(setf (obstacle test-world) (append (obstacle test-world) (list(list (nth 0 a) (nth 1 a)))))
          )
         ((eq 'M p);this creats the size of the map
          (create-World-list (nth 1 a) (nth 2 a) (nth 3 a))
          (setq worldSize (list (nth 1 a) (nth 2 a)))
          )
         ((eq 'R p) ;this creates an agent ROBOT
          ;The input
          (defvar p (create-agent (nth 1 a) (nth 2 a) (nth 3 a))))

        ))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This create a gobal variables for an instance of the agent class
;;; to be access for the other fuctions.
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-agent(cmax alt loca)
         ;create an agent at a posiion and altitude, make sure altitude is correct when placing agent, location input is in a (list x y) form
   (make-instance 'agent :climb-max cmax :altitude alt :GPS-location loca)

  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This create a map of the world for both the actual world and
;;; the agent map. The agent map will always be instanitated with 
;;; 0 for height. 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;------------------------------------------------------------------------------------------------------------------------------------
;;creates the list of the world in the form of (x y z) z being the height
(defun create-World-list (x y z)
  (setf tempZ z)
  (setf tempWorld '())
  (setf tempWorld2 '())
  (setf agentMap '())
  (setf count1 0)
  (dotimes (n x)
    (dotimes (n2 y)
      (setq tempWorld2 (append tempWorld2 (list(list n n2 0))))
      (setq agentMap tempWorld2)
      (setq tempWorld (append tempWorld (list(list n n2 (nth count1 tempZ)))))
      (setq worldMap tempWorld)
      (setf count1 (+ 1 count1))
      )
    )
  (setf count1 0)
  (setf (know p) agentMap)
  )

;(setf (know p) agentMap)
;;set the agent object with a list of it's own world

;------------------------------------------------------------------------------------------------------------------------------------
;This print the list of the world inputed into the function (map, sizeofmap)
;------------------------------------------------------------------------------------------------------------------------------------
(defun print-world(x size)
  (setf size1 (nth 0 size))
  (setf size2 (nth 1 size))
  (setf count1 0)
  (dotimes (n size1)
    (format t "~%")
    (dotimes(n2 size2)
    (format t "~d"(nth count1 x ))
    (setf count1 (+ 1 count1))

  ))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; this update the agent knowleage of its own world and is used when 
;;; following a path to update.
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;n
(defun update-agent-world (world-map agent-map size currentPos)
  (setf rowSize (nth 0 worldSize))
  (setf Tempcount 0)
  (loop for a in world-map
    do(cond
      ((equalp currentPos a)
          ;checkPos to the worldMap
          (ignore-errors
          (if(not (equalp (nth Tempcount agent-map) (nth Tempcount world-map)))
            (progn
            (print "update current postion")
            (ignore-errors(setf (nth 2(nth Tempcount agent-map)) (nth 2 (nth Tempcount world-map)))))
            )
          ;check if down is updated
          (if(not(equalp (nth (+ Tempcount rowSize) agent-map) (nth (+ Tempcount rowSize) world-map)))
            ;check down if not already updated
            (if (and (<=(+ Tempcount 1) size) (>=(+ Tempcount 1) 0))
              (progn
                (print "Checking Down")
     			  (setf (nth 2 (nth (+ Tempcount rowSize) agent-map))(nth 2 (nth (+ Tempcount rowSize) world-map)))
                )))     
          (if(not(equalp (nth (+ Tempcount 1) agent-map) (nth (+ Tempcount 1) world-map)))
            ;check left
            (if (and (<=(+ Tempcount 1) size) (>=(+ Tempcount 1) 0))
              (progn
                (print "Checking Left")
             (setf(nth 2 (nth (+ Tempcount 1) agent-map))(nth 2 (nth (+ Tempcount 1) world-map)))
                )))
          (if(not(equalp (nth (- Tempcount 1) agent-map) (nth (- Tempcount 1) world-map)))
          ;check right
            (if (and (<=(- Tempcount 1) size) (>=(- Tempcount 1) 0))
              (progn
                (print "Checking Right")
               (setf(nth 2 (nth (- Tempcount 1) agent-map))(nth 2 (nth (- Tempcount 1) world-map)))

                )))
          (if(not(equalp (nth (- Tempcount rowSize) agent-map) (nth (- Tempcount rowSize) world-Map)))
          ;;check up
            (if (and (<=(- Tempcount rowSize) size) (>=(- Tempcount rowSize) 0))
              (progn
                (print "Checking Up")
                (setf (nth 2 (nth (- Tempcount rowSize) agent-map)) (nth 2 (nth (- Tempcount rowSize) world-map)))
                ))))
        
        )
      )
      ;count to grab the position of the currentPOS
    do(setf Tempcount (+ 1 Tempcount))
      ;reset the count
      )(setf Tempcount 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This should set the agent for the height of it current postition
;;; and update the curret spot for the first time
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-agent (world-map agent-map)
;;update the agent
(setf x (nth 0 (loc p)))
(setf y (nth 1 (loc p)))
;grab current postion(x y z)
(setf (loc p) (grab-currentPOS-height x y world-map))
(update-agent-world world-Map agent-map (list-length world-map) (loc p))
;check which node is srrounding height, it max height

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; used in the setup-agent
;;; This is used to grab the height of the agents starting postion
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grab-currentPOS-height(x y world-map)
  (loop for a in world-map
    do(cond
      ((and (= y (nth 1 a)) (= x (nth 0 a)) )
        (return a)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function runs the path given by the search in a list of (x,y)
;;; format list. ex: ((x,y) (x,y).....)
;;; This will run until it reaches the goal or is stuck from height 
;;; being larger then the max height given.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun follow-path (world-map agent-map path goal)
  (setf Newpath path)
  (setf tempcountNew 0)
  (setf lastPosition)
  (setf rowSizeNew (nth 0 worldSize))
  (setup-agent world-map agent-map)
  (print-world (know p) worldsize)
 
  (loop while(not (equalp (list (nth 0 (loc p)) (nth 1 (loc P))) goal))
    ;loop in the world map
    do(loop for num in agent-map

      do(cond
        ;create list of (x,y) then compare with list of (x,y)

        ((and(equalp(list(nth 0 (loc p))(nth 1 (loc p))) (nth 0 Newpath)) (equalp (loc p) (nth tempcountNew world-map)))
          
            (ignore-errors
              ;check right of the agent map
            (if(and (equalp (list (nth 0 (nth (+ tempcountNew 1) agent-map))  (nth 1 (nth (+ tempcountNew 1) agent-map))) (nth 1 Newpath)) 
              (<= (nth 2 (nth (+ tempcountNew 1) agent-map))) (c-max p))
              (progn
              (print (loc p))
              (print "Moving Right")
              (setf (loc p) (nth (+ tempcountNew 1) agent-map))
              (update-agent-world world-map agent-map (list-length agent-map) (loc p))
              (print-world (know p) worldsize)
              (setf Newpath (cdr Newpath))
              
              )
              )

            (if(and (equalp (list (nth 0 (nth (- tempcountNew 1) agent-map))  (nth 1 (nth (- tempcountNew 1) agent-map))) (nth 1 Newpath)) 
              (<= (nth 2 (nth (- tempcountNew 1) agent-map))) (c-max p))
              (progn
              (print (loc p))
              (print "Moving Left")
              (setf (loc p) (nth (- tempcountNew 1) agent-map))
              (update-agent-world world-map agent-map (list-length agent-map) (loc p))
              (print-world (know p) worldsize)
              (setf Newpath (cdr Newpath))
             
              )
              )


            (if(and (equalp (list (nth 0 (nth (+ tempcountNew rowSizeNew) agent-map)) (nth 1 (nth (+ tempcountNew rowSizeNew) agent-map)))
             (nth 1 Newpath)) (<= (nth 2 (nth (+ tempcountNew 1) agent-map))) (c-max p))
              (progn
              (print (loc p))
              (print "Moving Down")
              (setf (loc p) (nth (+ tempcountNew rowSizeNew) agent-map))
              (update-agent-world world-map agent-map (list-length agent-map) (loc p))
              (print-world (know p) worldsize)
              (setf Newpath (cdr Newpath))
              
              )
              )

            (if(and (equalp (list (nth 0 (nth (- tempcountNew rowSizeNew) agent-map))  (nth 1 (nth (- tempcountNew rowSizeNew) agent-map))) (nth 1 Newpath))
             (<= (nth 2 (nth (- tempcountNew 1) agent-map))) (c-max p))
              (progn
              (print (loc p))
              (print "Moving Up")
              (setf (loc p) (nth (- tempcountNew rowSizeNew) agent-map))
              (update-agent-world world-map agent-map (list-length agent-map) (loc p))
              (print-world (know p) worldsize)
              (setf Newpath (cdr Newpath))
              
              )
              )

            )

          )


        ((not(equalp (loc p) (nth tempcountNew world-map)))

          (print "fuck")


          )



        );end of do
      do(if(<= tempcountNew (list-length agent-map))
            (setf tempcountNew (+ 1 tempcountNew)))

      )

    do(setf tempcountNew 0)
    )

   (print (loc p))
   (print "found Goal")

)

(defun input-world()
(input)                        ;this automaticly read in the file
(read_input object)
)



(defun Start()
  (setf y '((0 0)(0 1)(0 2)(4 1)(1 3)(2 3)(3 3)(3 4)(4 4)))
  ;(setf y '((0 0)(0 1)(0 2)(0 3)(1 3)(2 3)(3 3)(3 4)(4 4)))
  (follow-path worldMap (know p) y goal)
 

)


