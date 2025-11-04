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

(defstruct (mp3-file (:conc-name mp3-))
  "A structure containing the PCM data decoded from an MP3 file as well as metadata about the MP3 file.
The left and right channel data is in a format usable by cl-fftw."
  (left-channel)
  (right-channel)
  (samples)
  (element-type 'double-float)
  (sample-rate 44100 :type (unsigned-byte 32))
  (channels 2 :type (unsigned-byte 32))
  (mpg123-type 208 :type (unsigned-byte 32)))

(defun mp3-duration-in-seconds (mp3)
  "Compute the duration of an mp3-file in seconds."
  (with-slots (samples channels sample-rate) mp3
    (/ (length samples)
       (* channels sample-rate))))

(defun mp3-sample-index-for-time (mp3 time-in-seconds)
  "Calculate an index into samples for the specified time in the playback."
  (declare (optimize (speed 3))
           (type mp3-file mp3)
           (type number time-in-seconds))
  (with-slots (samples channels sample-rate) mp3
    (* channels (floor (* time-in-seconds sample-rate)))))

(defun mp3-channel-index-for-time (mp3 time-in-seconds)
  "Calculate an index into left-channel or right-channel arrays
for the specified time in the playback."
  (declare (optimize (speed 3))
           (type mp3-file mp3)
           (type number time-in-seconds))
  (with-slots (samples channels sample-rate) mp3
    (* (floor (* time-in-seconds sample-rate)))))


;; Passing in element type causes some optimization notes.
;; I *think* it's better to get those warnings here and return
;; arrays with a specific type for downstream processing than it
;; would be to return a preset type and convert later.
;; TODO: This could be more flexible, but I don't know what the API should
;;       look like yet.
;;       For example, I might want to read left/right channels into real/imag
;;       components of a complex, or into a vec2, etc.
;;       Maybe having the channels broken out into their own arrays is doing too much?
(defun read-mp3-file (fname
                      &key
                        (scale-factor (/ 1 32768.0))
                        (element-type 'double-float))
  "Read the specified mp3 file into an mp3-file structure."

  (declare (optimize (speed 2) (safety 2) (debug 2)))
  ;; channels will be interleaved  left, right, left, right, ...
  (multiple-value-bind (samples sample-rate channels mt)
      (mpg123:decode-mp3-file (uiop:native-namestring fname)
                              :character-encoding :utf-8)

    (declare (type fixnum channels sample-rate)
             (type (simple-array (signed-byte 16) *) samples ))

    ;; Pull channel data out into independent left and right channels
    (loop
      :with samples-per-channel = (the fixnum (/ (length samples)
                                                 channels))
      :with left-channel = (make-array samples-per-channel
                                       :element-type element-type
                                       :initial-element (coerce 0.0 element-type))
      :with right-channel = (make-array samples-per-channel
                                        :element-type element-type
                                        :initial-element (coerce 0.0 element-type))
      :for i :below samples-per-channel
      :for left-raw :of-type number = (* scale-factor
                                         (aref samples (* 2 i)))
      :for right-raw :of-type number = (* scale-factor
                                          (aref samples (+ 1 (* 2 i))))
      :do
         (setf (aref left-channel i)
               (coerce left-raw element-type))
         (setf (aref right-channel i)
               (coerce right-raw element-type))
      :finally
         (return (make-mp3-file :samples samples
                                :element-type element-type
                                :left-channel left-channel
                                :right-channel right-channel
                                :sample-rate sample-rate
                                :channels channels
                                :mpg123-type mt)))))
