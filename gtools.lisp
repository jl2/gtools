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

(defun maybe-open (filename &key (with :emacs))
  "Open filename in an external application.
:with :emacs will (find-file-other-window filename)
:with \"application\" open with application
:with t opens with xdg-open, start, or open
:with nil doesn't open.
"
  (cond ((eq :emacs with)
         #+swank (swank:eval-in-emacs `(find-file-other-window ,(namestring filename)))
         #+swank (swank:eval-in-emacs `(previous-window-any-frame))
         #-swank (error "Can't open in Emacs without Swank."))

        ((stringp with)
         (uiop:launch-program (format nil "~a ~s &" with (namestring filename))))

        ((eq t with)
         (uiop:launch-program (format nil "xdg-open ~s &" (namestring filename))))))

(defun normalize-and-create-directories (path)
  "Utility function to make relative path names relative to the user's home directory to work with Cairo."
  (let ((rn (uiop:native-namestring path)))
    (ensure-directories-exist rn)
    rn))


(defun make-movie (directory
                   &key
                     (bit-rate (* 48 1024 1024))
                     (output-file-name (merge-pathnames directory
                                                        (make-pathname :name "movie"
                                                                       :type "mp4")))
                     (mp3-name nil)

                     (temp-name (merge-pathnames directory
                                                 (make-pathname :name "soundless"
                                                                :type "mp4")))
                     (file-template "frame%08d")
                     (image-type "png")
                     (remove-temp t))
  "Run ffmpeg to create a movie with audio."
  (when (probe-file temp-name)
    (delete-file temp-name))

    ;; Why is this in two steps?
  (let ((movie-command
          (format nil
                  "ffmpeg -r 30 -i \"~a~a.~a\" -b ~a -q 4 \"~a~a\""
                  (namestring directory)
                  file-template
                  image-type
                  bit-rate
                  (namestring directory)
                  (namestring temp-name)))

        (audio-command
          ;; If mp3-name has a value then the audio-command adds audio and writes the video to output-file-name
          (if mp3-name
              (format nil
                      "ffmpeg -i \"~a~a\" -i \"~a\" -codec copy -shortest \"~a~a\""
                      (namestring directory)
                      (namestring temp-name)
                      (namestring mp3-name)
                      (namestring directory)
                      (namestring output-file-name))
              ;; otherwise it just moves the tmp video to output-file-name
              (format nil "mv \"~a~a\" \"~a~a\""
                      (namestring directory)
                      (namestring temp-name)
                      (namestring directory)
                      (namestring output-file-name)))))
    (format t "Running: ~a~%" movie-command)
    (uiop:run-program movie-command)

    (when (probe-file output-file-name)
      (delete-file output-file-name))

    (format t "Running: ~a~%" audio-command)
    (uiop:run-program audio-command)

    ;; Only remove the temp if the final output file was created
    (when (and remove-temp
               (probe-file output-file-name))
      (delete-file temp-name))))
