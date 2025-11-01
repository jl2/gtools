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

(defpackage :gtools

  (:nicknames :gt)
  (:local-nicknames (#:c #:cl-cairo2))
  (:use #:cl
        #:alexandria
        #:3d-vectors
        #:3d-matrices
        )

  (:export #:maybe-open
           #:normalize-and-create-directories

           #:make-movie

           #:make-mat4
           #:make-quaternion
           #:qrotate
           #:q+
           #:q-
           #:q*
           #:q=
           #:qinv
           #:vec3-quaternion-rotate

           #:read-mp3-file
           #:mp3-file
           #:mp3-file-left-channel
           #:mp3-file-right-channel
           #:mp3-file-element-type
           #:mp3-file-sample-rate
           #:mp3-file-samples
           #:mp3-file-channels
           #:mp3-file-mpg123-type
           #:mp3-file-duration-in-seconds

           ))
