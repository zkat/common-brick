(defpackage #:common-brick
  (:use :cl :squirl :uid :sheeple))
(in-package :common-brick)

;;; Utilities
(defmacro fun (&body body)
  `(lambda (&optional _)
     (declare (ignorable _))
     ,@body))

;;; Resources
(defvar *resource-directory*
  (merge-pathnames "res/" (load-time-value (or #.*compile-file-truename* *load-truename*))))

;;;
;;; Physics callback
;;;
;;; - We use a custom collision callback because our game-objects are Sheeple-based. By default,
;;;   SquirL uses a CLOS-based multi-dispatched genfun (COLLIDE), which this emulates.
(defmessage collide-objects (a b arbiter)
  (:documentation "Collides A and B. If a reply returns NIL, the collision is not resolved by
SquirL. Otherwise, the collision actually happens. The body of the reply is executed regardless."))

;;;
;;; Engine
;;;
;;; - UID applications should define their own engine prototypes. This allows users
;;;   to define some default behaviors, as well as move data that would otherwise be
;;;   global into a convenient single object.
(defproto =common-brick= (=engine=)
  ((uid:title "Common Brick")
   (uid:window-width 800)
   (uid:window-height 600)
   (uid:clear-color uid:*white*)
   current-level))

(defreply init :after ((game =common-brick=))
  (setf (current-level game) (gen-level)))

(defreply draw ((game =common-brick=) &key)
  (when (current-level game)
    (draw (current-level game))))
(defreply update ((game =common-brick=) dt &key)
  (when (current-level game)
    (update (current-level game) dt)))

;;;
;;; Game objects
;;;
;;; - UID sprites do not hold position information, and squirl bodies do not hold graphics information.
;;;   The clear solution is to just put them both into a "game object" which is what we will
;;;   actually be interacting with.
(defproto =game-object= ()
  (graphic physics-body))

(defmessage object-position (obj)
  (:reply ((obj =game-object=)) (body-position (property-value obj 'physics-body))))

(defmessage (setf object-position) (new-value obj)
  (:reply (new-value (obj =game-object=))
    (setf (body-position (property-value obj 'physics-body)) new-value)))

(defreply draw ((object =game-object=) &rest args &key)
  (with-properties (graphic physics-body) object
    (let ((vec (body-position physics-body)))
      ;; todo: draw the rotation, too.
      (apply 'draw-at (vec-x vec) (vec-y vec) graphic args))))

(defreply update ((object =game-object=) (dt =number=) &key) nil)

;; A basic breakout game involves 3 "game objects": A bunch of bricks, one or more balls,
;; and one or more paddles. We create prototypes for each of these 3 types. In this particular
;; case, delegation makes sharing the image resource easy and transparent.
(defproto =brick= (=game-object=)
  ((graphic (create-image (merge-pathnames "brick.png" *resource-directory*))) destroyedp))
;; For each prototype, we also need to provide an init-object reply that takes care of adding
;; the actual physics body to each object. Each physics body has to be unique, so we can't just
;; stuff it in the prototype.
(defreply init-object :after ((obj =brick=) &key)
  (setf (physics-body obj)
        ;; bricks are static bodies, so we don't provide a mass.
        (make-body :actor obj
                   :shapes (list (make-rectangle (width (graphic obj)) (height (graphic obj))
                                                 :restitution 0.8
                                                 ;; Friction will make the ball rotate when
                                                 ;; it strikes at an angle.
                                                 :friction 0.3)))))
(defreply update ((brick =brick=) dt &key)
  (declare (ignore dt))
  (when (destroyedp brick)
    (world-remove-body (physics-world (current-level *engine*)) (physics-body brick))
    (setf (bricks (current-level *engine*)) (remove brick (bricks (current-level *engine*))))))

;; Rinse and repeat for the other object types...
(defproto =paddle= (=game-object=)
  ((graphic (create-image (merge-pathnames "paddle.png" *resource-directory*)))
   (delta-x 0) (velocity 200)))
(defreply init-object :after ((obj =paddle=) &key)
  (let* ((width (width (graphic obj)))
         (height (height (graphic obj)))
         ;; Note that width/height for UID images defaults to 0 unless the engine
         ;; is already running, so this will only be accurate if OBJ is created
         ;; inside a running engine.
         (point-a (vec (- (/ width 2)) 0))
         (point-b (vec (/ width 2) 0)))
    (setf (physics-body obj)
          ;; The ACTOR slot in a body determines what object COLLIDE-OBJECTS will be called with
          ;; when a collision between two shapes happens. In the case of our game objects,
          ;; we want to write COLLIDE-OBJECTS replies that dispatch on the game objects themselves,
          ;; so we set the physics body's actor to each object.
          (make-body :actor obj :shapes (list (make-segment point-a point-b
                                                            :radius (/ height 2) :friction 0.8
                                                            ;; This should make it so balls speed up
                                                            ;; slightly every time they hit a paddle
                                                            ;; (since "bounce" is a little more
                                                            ;; than perfect)
                                                            :restitution 1.1))))))

(defreply update ((paddle =paddle=) dt &key)
  (with-properties (physics-body delta-x velocity) paddle
    (when (key-down-p :right) (incf delta-x))
    (when (key-down-p :left) (decf delta-x))
    (body-slew physics-body (vec (+ (* delta-x velocity dt)
                                    (vec-x (body-position physics-body)))
                                 (vec-y (body-position physics-body)))
               (float dt 1d0))
    (setf delta-x 0)))

;; And now our balls...
(defproto =ball= (=game-object=)
  ((graphic (create-image (merge-pathnames "ball.png" *resource-directory*)))))
(defreply init-object :after ((obj =ball=) &key)
  (let* ((radius (/ (width (graphic obj)) 2)))
    (setf (physics-body obj)
          (make-body :actor obj
                     ;; Balls are our only non-static objects, so they have some mass.
                     :mass 5 :velocity (vec (random 100) 400)
                     :shapes (list (make-circle radius :friction 0.7 :restitution 1))))))

(defreply key-down :after ((game =common-brick=) key)
  (when (eq key #\Space)
    (let ((ball (create =ball=)))
      (setf (object-position ball)
            (object-position (car (paddles (current-level game)))))
      (push ball (balls (current-level game)))
      (world-add-body (physics-world (current-level game))
                      (physics-body ball)))))

;;;
;;; Level
;;;
(defparameter *dt-threshold* 0.5)
(defproto =level= ()
  ((physics-world (make-world :damping 5d0 :collision-callback #'collide-objects))
   (accumulator 0) (physics-timestep (float 1/100 1d0))
   bricks paddles balls))
(defreply init-object :after ((level =level=) &key)
  (setf (physics-world level) (make-world :gravity (vec 0 -200)
                                          :collision-callback #'collide-objects)))

(defreply draw ((level =level=) &key)
  (with-properties (bricks paddles balls) level
    (map nil #'draw bricks)
    (map nil #'draw paddles)
    (map nil #'draw balls)))

(defreply update ((level =level=) dt &key)
  (let ((update-fun (fun (update _ dt))))
    (with-properties (bricks paddles balls physics-world physics-timestep accumulator) level
      (map nil update-fun paddles)
      (map nil update-fun balls)
      (map nil update-fun bricks)
      ;; now update the physics world
      (squirl::rehash-world-static-data physics-world)
      (incf accumulator (min *dt-threshold* dt))
      (loop while (>= accumulator physics-timestep) do
           (map nil (fun (body-update-position (physics-body _) physics-timestep)) paddles)
           (world-step physics-world physics-timestep)
           (decf accumulator physics-timestep)))))

(defun gen-level (&aux (level (create =level=)))
  (with-properties (bricks paddles balls physics-world) level
    ;; add the walls first
    (world-add-body physics-world (make-body :shapes (list (make-segment (vec 0 0) (vec 0 600)
                                                                         :restitution 0.2)
                                                           (make-segment (vec 0 600) (vec 800 600)
                                                                         :restitution 0.2)
                                                           (make-segment (vec 800 600) (vec 800 0)
                                                                         :restitution 0.2)
                                                           (make-segment (vec 800 0) (vec 0 0)
                                                                         :restitution 0.2))))
    (push (create =paddle=) paddles)
    (setf (object-position (car paddles)) (vec 400 30))
    (loop for x from 25 by 50 upto 800 do
         (loop for y from 310 by 20 upto 600
            for brick = (create =brick=) do
              (setf (object-position brick) (vec x y))
              (push brick bricks)))
    (let ((world-add-fun (fun (world-add-body physics-world (physics-body _)))))
      (map nil world-add-fun paddles)
      (map nil world-add-fun balls)
      (map nil world-add-fun bricks)))
  level)

;;;
;;; Game object collisions
;;;
(defreply collide-objects (a b arbiter) (declare (ignore a b arbiter)) t)
;; Now we define the actual replies...
(defreply collide-objects ((obj1 =ball=) (obj2 =ball=) (arbiter =t=)) t)
(defreply collide-objects ((obj1 =paddle=) (obj2 =ball=) (arbiter =t=)) t)
(defreply collide-objects ((obj1 =ball=) (obj2 =paddle=) (arbiter =t=)) t)
(defreply collide-objects ((obj1 =brick=) (obj2 =ball=) (arbiter =t=))
  (setf (destroyedp obj1) t))
(defreply collide-objects ((obj1 =ball=) (obj2 =brick=) (arbiter =t=))
  (setf (destroyedp obj2) t))
