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

(setq mode-line-format '(:eval (nth nyan-lite-time nyan-lite-timeline)))

(defun nyan-lite-next-frame ()
  (setq nyan-lite-time (if (>= nyan-lite-time 1) 0 (1+ nyan-lite-time))))

(nyan-lite-next-frame)
(force-mode-line-update)

(setq nyan-lite-timer (run-at-time 0 0.6 #'nyan-lite-next-frame))

(cancel-timer nyan-lite-timer)

(provide 'nyan-lite)

;;; nyan-lite.el ends here
