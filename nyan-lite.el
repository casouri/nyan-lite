;;; nyan-lite.el --- Nyan Mode Lite      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;

;;; Code:
;;


(setq nyan-lite-timeline '("=====" "-----"))
(setq nyan-lite-time 0)
(setq nyan-lite-rainbow-image (concat default-directory "img/rainbow.xpm"))
(setq nyan-lite-nyan-image (concat default-directory "img/nyan.xpm"))

;; SCRATCH
(setq mode-line-format '(:eval (nth nyan-lite-time nyan-lite-timeline)))


(defun nyan-lite-next-frame ()
  "Increment `Nyan-lite-time' so mode-line will display next frame next time."
  (setq nyan-lite-time (if (>= nyan-lite-time 1) 0 (1+ nyan-lite-time))))


(nyan-lite-next-frame)
(force-mode-line-update)
;; END_SCRATCH

(setq nyan-lite-timer (run-at-time 0 0.6 #'nyan-lite-next-frame))

;; SCRATCH
(cancel-timer nyan-lite-timer)
(insert (propertize "1" 'display (create-image nyan-lite-rainbow-image 'xpm nil :ascent 90)))111
;; END_SCRATCH

(setq nyan-lite-trail-ascent-pattern '#1=(90 . (90 . (100 . (100 . #1#)))))

;; SCRATCH
(nth 100 nyan-lite-trail-ascent-pattern)
;; END_SCRATCH

(defun nyan-lite-build-trail (time length)
  "There are four different possible trail pattern depends on TIME.
TIME can be 0, 1, 2, 3.
Let - represent high rainbow, _ represent low rainbow:
When TIME is 0, pattern is ...__--__-- (right to left)
When TIME is 1, pattern is ..._--__--__
When TIME is 2, pattern is ...--__--__
When TIME is 3, pattern is ...-__--__--

LEGTH is the length of the trail in units of a rainbow segment (8 pixels)."
  (let ((index time)
        (end-index+1 (+ time length)) ; keep the total length
        trail)
    (while (< index end-index+1)
      (push (propertize "=" 'display
                        (create-image nyan-lite-rainbow-image 'xpm nil
                                      ;; nyan-lite-trail-ascent-pattern is a circular list
                                      ;; with repeated pattern of 90 90 100 100
                                      :ascent (nth index nyan-lite-trail-ascent-pattern)))
            trail)
      (incf index))
    (cl-reduce 'concat (reverse trail))))

;; SCRATCH
(insert (nyan-lite-build-trail 3 10))
;; END_SCRATCH

(defvar nyan-lite-nyan-file-list (mapcar (lambda (num)
                                           (format "%simg/nyan-frame-%d.xpm" default-directory num))
                                         '(1 2 4 6))
  "File names of each frame of nyan cat.")

(defun nyan-lite-build-frame (time length)
  "Return Nan cat with trails of LENGTH.
TIME represents the frame in time line.
Each frame is slightly different to make nyan cat animate.
TIME can be 0, 1, 2, 3."
  (unless (member time '(0 1 2 3)) (error "TIME can only be 0, 1, 2, 3, not %d" time))
  (concat (nyan-lite-build-trail time (1- length))
          (propertize ">" 'display (create-image (nth time nyan-lite-nyan-file-list) 'xpm nil
                                                 ;; make nyan go up and down with trail
                                                 ;; Since xmp of nyan cat comes with a little bit of
                                                 ;; trail already, we can make the trail and cat
                                                 ;; stagger by 1 frame
                                                 :ascent (nth (1+ time) nyan-lite-trail-ascent-pattern)))))

;; SCRATCH
(insert (nyan-lite-build-frame 0 10))
;; END_SCRATCH

(defun nyan-lite-build-timeline (width)
  "Build a timeline of nyan-cat.
Each element of the returned list(timeline) is a string propertied with image.
WIDTH is in terms of 8 pixel units."
  (list
   (nyan-lite-build-frame 0 width)
   (nyan-lite-build-frame 1 width)
   (nyan-lite-build-frame 2 width)
   (nyan-lite-build-frame 3 width)))

(setq nyan-lite-timeline (nyan-lite-build-timeline 10))


(defun nyan-lite-next-frame ()
  "Increment `Nyan-lite-time' so mode-line will display next frame next time."
  (setq nyan-lite-time (if (>= nyan-lite-time 3) 0 (1+ nyan-lite-time))))

;;; Public Stuff

(defun nyan-lite-mode-line ()
  "Return the nyan lite segment for mode-line.
Intended to use in `mode-line-fprmat': (:eval (nyan-lite-mode-line))"
  (nth nyan-lite-time nyan-lite-timeline))




(provide 'nyan-lite)

;;; nyan-lite.el ends here
