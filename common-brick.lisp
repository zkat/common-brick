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

;;; Physics callback
(defmessage collide-objects (a b contacts))

;;; The engine object
(defproto =common-brick= (=engine=)
  ((uid:title "Common Brick")
   (uid:window-width 800)
   (uid:window-height 600)
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
  (let ((fun (fun (update _ dt))))
    (with-properties (bricks paddles balls) game
      (map nil fun paddles)
      (map nil fun balls)
      (map nil fun bricks))))

;;; Game objects
(defproto =game-object= ()
  ((x 0) (y 0) graphic physics-body))

(defproto =brick= (=game-object=)
  ((graphic (create-image (merge-pathnames "brick.png" *resource-directory*)))))
(defreply init-object :after ((obj =brick=) &key)
  (let* ((height/2 (/ (height (graphic obj)) 2))
         (width/2 (/ (width (graphic obj)) 2))
         (verts (list (vec (- width/2) height/2)
                      (vec width/2 height/2)
                      (vec width/2 (- height/2))
                      (vec (- width/2) (- height/2)))))
    (setf (physics-body obj)
          (make-body :actor obj
                     :shapes (list (make-poly verts :friction 0.3))))))

(defproto =paddle= (=game-object=)
  ((graphic (create-image (merge-pathnames "paddle.png" *resource-directory*)))))
(defreply init-object :after ((obj =paddle=) &key)
  (let* ((width (width (graphic obj)))
         (height (height (graphic obj)))
         (point-a (vec (- (/ width 2)) 0))
         (point-b (vec (/ width 2) 0)))
    (setf (physics-body obj)
          (make-body :actor obj
                     :shapes (list (make-segment point-a point-b
                                                 :radius height
                                                 :restitution 1.001
                                                 :friction 0.6))))))

(defproto =ball= (=game-object=)
  ((graphic (create-image (merge-pathnames "ball.png" *resource-directory*)))))
(defreply init-object :after ((obj =ball=) &key)
  (let* ((radius (width (graphic obj))))
    (setf (physics-body obj)
          (make-body :actor obj
                     :mass 5
                     :shapes (list (make-circle radius :friction 0.3 :restitution 0.3))))))

(defreply draw ((object =game-object=) &rest args &key)
  (with-properties (x y content) object
    (apply 'draw-at x y content args)))

(defreply update :before ((object =game-object=) dt &key)
  (update (graphic object) dt))
