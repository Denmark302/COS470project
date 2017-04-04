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

  