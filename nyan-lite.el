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
;; A animated nyan cat on mode-line, nothing else.

;;; Code:
;;

(require 'cl-lib)

;; SCRATCH
;; (setq nyan-lite-timeline '("=====" "-----"))
;; (setq nyan-lite-time 0)
;; (setq nyan-lite-rainbow-image (concat default-directory "img/rainbow.xpm"))
;; (setq nyan-lite-nyan-image (concat default-directory "img/nyan.xpm"))

;; (setq mode-line-format '(:eval (nth nyan-lite-time nyan-lite-timeline)))

;; (defun nyan-lite-next-frame ()
;;   "Increment `Nyan-lite-time' so mode-line will display next frame next time."
;;   (setq nyan-lite-time (if (>= nyan-lite-time 1) 0 (1+ nyan-lite-time))))

;; (nyan-lite-next-frame)
;; (force-mode-line-update)
;; END_SCRATCH

;; SCRATCH
;; (setq nyan-lite-timer (run-at-time 0 0.6 #'nyan-lite-next-frame))
;; (cancel-timer nyan-lite-timer)
;; (insert (propertize "1" 'display (create-image nyan-lite-rainbow-image 'xpm nil :ascent 90)))111
;; END_SCRATCH


;;; Variables

;;;; Public

(defvar nyan-lite-width 10
  "Width of nyan cat in mode-line in unit of 8 pixels.")

(defvar nyan-cat-animate-interval 0.6
  "The update interval of animation.")

(defvar nyan-lite-add-mode-line t
  "Whether to add nyan cat to mode-line automatically.")

(defvar nyan-lite-trail-ascent-pattern '#1=(90 90 100 100 . #1#)
  "A circular list of the pattern of trail: __--__--__.")

;;;; Private

(defvar nyan-lite-time 0
  "Time in timeline.")

(defvar nyan-lite-timeline nil)

(defvar nyan-lite-dir (file-name-directory (or load-file-name buffer-file-name)))

(defvar nyan-lite-rainbow-image (concat nyan-lite-dir "img/rainbow.xpm")
  "Path to nyan rainbow images")

(defvar nyan-lite-nyan-file-list (mapcar (lambda (num)
                                           (format "%simg/nyan-frame-%d.xpm" nyan-lite-dir num))
                                         '(1 2 4 6))
  "File names of each frame of nyan cat.")

(defvar nyan-lite-timer nil
  "Timer for nyan-lite.")

;; SCRATCH
;; (nth 100 nyan-lite-trail-ascent-pattern)
;; END_SCRATCH

;;; Functions

;;;; Public

(defun nyan-lite-mode-line ()
  "Return the nyan lite segment for mode-line.
Intended to use in `mode-line-fprmat': (:eval (nyan-lite-mode-line))"
  (nth nyan-lite-time nyan-lite-timeline))


(define-minor-mode nyan-lite-mode
  "Nyan Lite mode, display a animted nyan cat in mode-line."
  :lighter "CAT"
  :global t
  (if nyan-lite-mode
      (progn
        (when nyan-lite-add-mode-line (add-to-list 'mode-line-format '(:eval (nyan-lite-mode-line)) t))
        (setq nyan-lite-timer (run-at-time 0 nyan-cat-animate-interval #'nyan-lite-next-frame)))
    (cancel-timer nyan-lite-timer)
    (when nyan-lite-add-mode-line
      (setq mode-line-format (remove '(:eval (nyan-lite-mode-line)) mode-line-format)))))

;;;; Private

(defun nyan-lite-next-frame ()
  "Increment `Nyan-lite-time' so mode-line will display next frame next time."
  (setq nyan-lite-time (if (>= nyan-lite-time 3) 0 (1+ nyan-lite-time)))
  (force-mode-line-update))


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
;; (insert (nyan-lite-build-trail 3 10))
;; END_SCRATCH

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
;; (insert (nyan-lite-build-frame 0 10))
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


;;; Load

(setq nyan-lite-timeline (nyan-lite-build-timeline 10))


(provide 'nyan-lite)

;;; nyan-lite.el ends here
