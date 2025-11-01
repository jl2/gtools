;; Copyright (c) 2025 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :gtools)

(declaim (inline q+ q- q* q= qinv
                 make-mat4
                 make-quaternion
                 qrotate
                 vec3-quaternion-rotate
                 ))

(defun make-quaternion ()
  "Return an identity quaternion."
  (vec4 0 0 0 1))

(defun q+ (a b)
  "Add quaternions a and b"
  (declare  (optimize (speed 3) (safety 0) (debug 0) (space 0))
            (type vec4 a b))
  (v+ a b))

(defun q- (a b)
  "Subtract quaternions a and b"
  (declare  (optimize (speed 3) (safety 0) (debug 0) (space 0))
            (type vec4 a b))
  (v- a b))

(defun q= (a b)
  "Check a and b for equality."
  (declare  (optimize (speed 3) (safety 0) (debug 0) (space 0))
            (type vec4 a b))
  (v= a b))

(defun q* (a b)
  "Multiply quaternions a and b."
  (declare  (optimize (speed 3) (safety 0) (debug 0) (space 0))
            (type vec4 a b))
  (declare  (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (let ((dot (v. (vxyz a) (vxyz b)))
        (cross (vc (vxyz a) (vxyz b))))
    (declare  (type #.3d-vectors::*float-type* dot)
              (type vec3 cross))
    (with-vec4 (ax ay az aw) a
      (with-vec4 (bx by bz bw) b
        (with-vec3 (cx cy cz) cross
          (vec4 (+  (* aw bx)
                    (* bw ax)
                    cx)
                (+  (* aw by)
                    (* bw ay)
                    cy)
                (+  (* aw bz)
                    (* bw az)
                    cz)
                (- (* aw bw)
                   dot)))))))

(defun make-mat4 (quat)
  "Convert a quaternion to a transformation matrix."
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0))
           (type vec4 quat))
  (let* ((xsq2 (* 2 (vx quat) (vx quat)))
         (ysq2 (* 2 (vy quat) (vy quat)))
         (zsq2 (* 2 (vz quat) (vz quat)))
         (sx (- 1 ysq2 zsq2))
         (sy (- 1 xsq2 zsq2))
         (sz (- 1 xsq2 ysq2)))
    (declare (type real xsq2 ysq2 zsq2 sx sy sz))

    (with-vec4 (x y z w) quat
      (mat4 `(,sx                           ;; 0
              ,(* 2 (+ (* x y) (* w z)))    ;; 1
              ,(* 2 (+ (* z x) (* -1 w y))) ;; 2
              0                             ;; 3
              ,(* 2 (+ (* x y) (* -1 w z))) ;; 4
              ,sy                           ;; 5
              ,(* 2 (+ (* y z) (* w x)))    ;; 6
              0                             ;; 7
              ,(* 2 (+ (* z x) (* w y)))    ;; 8
              ,(* 2 (+ (* y z) (* -1 w x))) ;; 9
              ,sz                           ;; 10
              0                             ;; 11
              0                             ;; 12
              0                             ;; 13
              0                             ;; 14
              1.0)                          ;; 15
            ))))

(defun qrotate (quaternion angle xyz)
  "Rotate quaternion by angle radians around the vec3 xyz."
  (declare  (optimize (speed 3) (safety 0) (debug 0) (space 0))
            (type single-float angle)
            (type vec3 xyz))
  (let ((half (* 0.5 angle))
        (sin-half (sin (* 0.5 angle))))
    (q* quaternion
              (vec4 (* (vx xyz) sin-half)
                    (* (vy xyz) sin-half)
                    (* (vz xyz) sin-half)
                    (cos half)))))

(defun qinv (quat)
  "Invert the given quaternion."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type vec4 quat))
  (let* ((len-sq (vsqrlength quat))
         (scale (if (> len-sq 0.01)
                    (/ 1.0 len-sq)
                    1.0)))

    (vec4 (* scale (- (vx quat)))
          (* scale (- (vy quat)))
          (* scale (- (vz quat)))
          (* scale (vw quat)))))

(defun vec3-quaternion-rotate (vec quat)
  "Rotate the vec3 vec by the quaternion quat."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type vec4 quat)
           (type vec3 vec))
  (let ((inv-q (qinv quat))
        (vq (vxyz_ vec)))
    (vxyz (q*
           (q* vq quat)
           inv-q))))
