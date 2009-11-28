(defpackage #:common-brick
  (:use :cl :squirl :uid :sheeple))
(in-package :common-brick)

(defvar *resource-directory*
  (merge-pathnames "res/" (load-time-value (or #.*compile-file-truename* *load-truename*))))

(defparameter *common-brick* (create-engine :title "Common Brick"
                                            :resizablep nil
                                            :window-width 800
                                            :window-height 600))

(defproto =game-object= ()
  ((x 0) (y 0) graphic))

(defproto =brick= (=game-object=)
  ((graphic (create-image (merge-pathnames "brick.png" *resource-directory*)))))

(defproto =paddle= (=game-object=)
  ((graphic (create-image (merge-pathnames "paddle.png" *resource-directory*)))))

(defproto =ball= (=game-object=)
  ((graphic (create-image (merge-pathnames "ball.png" *resource-directory*)))))

(defreply draw ((object =game-object=) &rest args &key)
  (with-properties (x y content) thing
    (apply 'draw-at x y content args)))

(defreply update :before ((object =game-object=) dt &key)
  (update (graphic object) dt))
