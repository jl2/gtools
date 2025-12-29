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
  (cond
    ((eq :emacs with)
     #+swank (swank:eval-in-emacs
              `(cond
                 ((get-buffer ,(namestring filename))
                  (revert-buffer (get-buffer ,(namestring filename))))
                 (t (find-file-other-window ,(namestring filename)))))
     #+swank (swank:eval-in-emacs
      `(previous-window-any-frame))
     #-swank (error "Can't open in Emacs without Swank."))
    ((eq :xnview with)
     (uiop:launch-program (format nil "xnview ~s &" (namestring filename))))

    ((stringp with)
     (uiop:launch-program (format nil "~a ~s &" with (namestring filename))))

    ((eq t with)
     (uiop:launch-program (format nil "xdg-open ~s &" (namestring filename))))))

(defun normalize-and-create-directories (path)
  "Utility function to make relative path names relative to the user's home directory to work with Cairo."
  (let ((rn (uiop:native-namestring path)))
    (ensure-directories-exist rn)
    rn))


(defun make-movie (image-file-directory
                   &key

                     (image-file-base "frame")
                     (image-file-digits 8)
                     (image-file-type "png")

                     (image-file-template (make-pathname
                                           :name (format nil
                                                         "~a%0~ad"
                                                         image-file-base
                                                         image-file-digits)
                                           :type image-file-type
                                           :defaults image-file-directory))

                     (output-file-name (make-pathname :name "movie"
                                                      :type "mp4"
                                                      :defaults image-file-directory))

                     (mp3-file-name nil)
                     (image-fps 12)
                     (final-fps 30)
                     (bit-rate (* 48 1024 1024))

                     (temp-file-name (make-pathname :name "soundless-temp-movie"
                                               :type "mp4"
                                               :defaults image-file-directory))
                     (delete-images t)
                     (delete-temp-file t))
  "Run ffmpeg to create a silent movie, then run it again to add an MP3 soundtrack.
The :image-file* parameters are used to build a template string for ffmpeg.

When :delete-images is t, files matching <directory>/<image-file-base>*.<image-file-type> are removed after creating the soundless MP4."
  (when (probe-file temp-file-name)
    (delete-file temp-file-name))

    ;; Why is this in two steps?
  (let ((movie-command
          (format nil
                  "ffmpeg -r ~d -i \"~a\" -b ~a -q 4 -r ~d \"~a\""
                  image-fps
                  (uiop:native-namestring image-file-template)
                  bit-rate
                  final-fps
                  (uiop:native-namestring temp-file-name)))

        (audio-command
          ;; If mp3-name has a value then the audio-command adds audio and writes the video to output-file-name
          (if mp3-file-name
              (format nil
                      "ffmpeg -r ~d -i \"~a\" -i \"~a\" -codec copy -shortest -r ~d \"~a\""
                      final-fps
                      (uiop:native-namestring temp-file-name)
                      (uiop:native-namestring mp3-file-name)
                      final-fps
                      (uiop:native-namestring output-file-name))
              ;; otherwise it just moves the tmp video to output-file-name
              (format nil "mv \"~a\" \"~a\""
                      (uiop:native-namestring temp-file-name)
                      (uiop:native-namestring output-file-name)))))
    (format t "Running: ~a~%" movie-command)
    (uiop:run-program movie-command :output *standard-output* :error *standard-output*)

    (when (probe-file output-file-name)
      (delete-file output-file-name))

    (format t "Running: ~a~%" audio-command)
    (uiop:run-program audio-command :output *standard-output* :error *standard-output*)

    ;; Only remove the temp if the final output file was created
    (when (and delete-temp-file
               (probe-file output-file-name))
      (delete-file temp-file-name))
    (when delete-images
      (let* ((all-file-path (make-pathname
                             :name (format nil
                                           "~a*"
                                           image-file-base)
                             :type image-file-type))
             (rm-cmd (format nil "find ~a -name ~s -exec rm {} \";\""
                             (uiop:native-namestring image-file-directory)
                             (uiop:native-namestring all-file-path)
                             )))
        (format t "Running: ~a~%" rm-cmd)
        (uiop:run-program rm-cmd :output *standard-output* :error *standard-output*)))))
