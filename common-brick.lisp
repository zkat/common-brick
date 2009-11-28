(defpackage #:common-brick
  (:use :cl :squirl :uid :sheeple))
(in-package :common-brick)

;;; Utilities
(defmacro fun (&body body)
  "This macro puts the FUN back in LAMBDA"
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
(defmessage collide-objects (a b contacts)
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
   bricks
   paddles
   balls
   (physics-world (make-world :collision-callback #'collide-objects))))

(defreply draw ((game =common-brick=) &key)
  (with-properties (bricks paddles balls) game
    (map nil #'draw bricks)
    (map nil #'draw paddles)
    (map nil #'draw balls)))

(defreply update ((game =common-brick=) dt &key)
  (let ((update-fun (fun (update _ dt))))
    (with-properties (bricks paddles balls) game
      (map nil update-fun paddles)
      (map nil update-fun balls)
      (map nil update-fun bricks))))

;;;
;;; Game objects
;;;
;;; - UID sprites do not hold position information, and squirl bodies do not hold graphics information.
;;;   The clear solution is to just put them both into a "game object" which is what we will
;;;   actually be interacting with.
(defproto =game-object= ()
  (graphic physics-body)
  (:documentation "Common Brick game object."))

;; A basic breakout game involves 3 "game objects": A bunch of bricks, one or more balls,
;; and one or more paddles. We create prototypes for each of these 3 types. In this particular
;; case, delegation makes sharing the image resource easy and transparent.
(defproto =brick= (=game-object=)
  ((graphic (create-image (merge-pathnames "brick.png" *resource-directory*)))))
;; For each prototype, we also need to provide an init-object reply that takes care of adding
;; the actual physics body to each object. Each physics body has to be unique, so we can't just
;; stuff it in the prototype.
(defreply init-object :after ((obj =brick=) &key)
  (setf (physics-body obj)
        ;; bricks are static bodies, so we don't provide a mass.
        (make-body :actor obj
                   :shapes (list (make-rectangle (width (graphic obj))
                                                 (height (graphic obj))
                                                 ;; Restitution of 1 gives "perfect" bounce.
                                                 ;; This'll make the ball(s) keep bouncing.
                                                 :restitution 1
                                                 ;; Friction will make the ball rotate when
                                                 ;; it strikes at an angle.
                                                 :friction 0.3)))))

;; Rinse and repeat for the other object types...
(defproto =paddle= (=game-object=)
  ((graphic (create-image (merge-pathnames "paddle.png" *resource-directory*)))))
(defreply init-object :after ((obj =paddle=) &key)
  (let* ((width (width (graphic obj)))
         (height (height (graphic obj)))
         ;; Note that width/height for UID images defaults to 1 unless the engine
         ;; is already running, so this will only be accurate if OBJ is created
         ;; inside a running engine.
         (point-a (vec (- (/ width 2)) 0))
         (point-b (vec (/ width 2) 0)))
    (setf (physics-body obj)
          ;; The ACTOR slot in a body determines what object COLLIDE-OBJECTS will be called with
          ;; when a collision between two shapes happens. In the case of our game objects,
          ;; we want to write COLLIDE-OBJECTS replies that dispatch on the game objects themselves,
          ;; so we set the physics body's actor to each object.
          (make-body :actor obj
                     :shapes (list (make-segment point-a point-b
                                                 :radius height
                                                 ;; This should make it so balls speed up
                                                 ;; slightly every time they hit a paddle
                                                 ;; (since "bounce" is a little more than perfect)
                                                 :restitution 1.01
                                                 :friction 0.6))))))

;; And now our balls...
(defproto =ball= (=game-object=)
  ((graphic (create-image (merge-pathnames "ball.png" *resource-directory*)))))
(defreply init-object :after ((obj =ball=) &key)
  (let* ((radius (width (graphic obj))))
    (setf (physics-body obj)
          (make-body :actor obj
                     ;; Balls are our only non-static objects, so they have some mass.
                     :mass 5
                     :shapes (list (make-circle radius :friction 0.3 :restitution 1))))))

;; Now that we have our game objects, we write some boilerplate to update and draw them.
(defreply draw ((object =game-object=) &rest args &key)
  (with-properties (graphic physics-body) object
    (let* ((vec (body-position physics-body))
           (x (vec-x vec))
           (y (vec-y vec)))
      ;; todo: draw the rotation, too.
      (apply 'draw-at x y graphic args))))

;; If we ever feel like making a game object's GRAPHIC an animation or such, this reply
;; will take care of updating it properly.
(defreply update :before ((object =game-object=) dt &key)
  (update (graphic object) dt))

;;;
;;; Game object collisions
;;;
;;; - SquirL has a DEFCOLLISION macro by default that abstracts away this nasty pattern.  Since
;;;   we're using a custom callback, though, we need to write this out ourselves :(
;;;
;;;   The reason we define functions separately is so that they can share a body and still close
;;;   over the lexical environment properly. tl;dr: I'm anal, and that's why this code is ugly.
;;;
(defun paddle/ball (paddle ball contacts)
  (declare (ignore paddle ball contacts))
  ;; for now, just return T, which means the collision happened.
  t)

(defun brick/ball (brick ball contacts)
  (declare (ignore brick ball contacts))
  ;; eventually, we want this to increment the score.
  t)

;; Now we define the actual replies...
(defreply collide-objects ((obj1 =ball=) (obj2 =ball=) contacts)
  (declare (ignore contacts))
  ;; with two balls involved, we can let them bounce. This reply is symmetrical, so we don't
  ;; need a separate function to have it share a body.
  t)

(defreply collide-objects ((obj1 =paddle=) (obj2 =ball=) contacts)
  (paddle/ball obj1 obj2 contacts))
(defreply collide-objects ((obj1 =ball=) (obj2 =paddle=) contacts)
  (paddle/ball obj2 obj1 contacts))

(defreply collide-objects ((obj1 =brick=) (obj2 =ball=) contacts)
  (brick/ball obj1 obj2 contacts))
(defreply collide-objects ((obj1 =ball=) (obj2 =brick=) contacts)
  (brick/ball obj2 obj1 contacts))
