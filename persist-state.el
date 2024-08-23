;;; persist-state.el --- Regularly persist bookmarks, history, recent files and more -*- lexical-binding: t; -*-

;; Copyright (C) 2023 - 2024 Bram Schoenmakers

;; Author: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Maintainer: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Created: 05 May 2023
;; Package-Version: 0.4
;; Package-Requires: ((emacs "28.2"))
;; Keywords: convenience
;; URL: https://codeberg.org/bram85/emacs-persist-state.git

;; This file is not part of GNU Emacs.

;; MIT License

;; Copyright (c) 2023 - 2024 Bram Schoenmakers

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package regularly saves your Emacs state (bookmarks, history,
;; recent files, etc.) when idle for a brief moment. In case Emacs is
;; idle for a longer time, no state is saved.

;; See README.org for more details.

;;; Code:

(require 'map)

(defgroup persist-state nil
  "Persist the Emacs state at regular intervals."
  :group 'convenience)

(defvar persist-state--save-state-timer nil
  "This variable holds the timer object to trigger regular saves.")

(defcustom persist-state-save-interval
  (* 15 60)
  "Interval (in seconds) to persist state."
  :type '(integer)
  :group 'persist-state)

(defcustom persist-state-wait-idle
  5
  "When it's time to save, wait for this amount of idle time (in seconds)."
  :type '(integer)
  :group 'persist-state)

(declare-function bookmark-save "ext:bookmark")
(defun persist-state--maybe-save-bookmarks ()
  "Save bookmarks if the built-in bookmark package is active."
  (when (bound-and-true-p bookmark-save-flag)
    (bookmark-save)))

(declare-function desktop-save "desktop")
(defvar desktop-path)
(defun persist-state--maybe-save-desktop ()
  "Save the desktop if the built-in desktop.el package is active."
  (when (bound-and-true-p desktop-save-mode)
    (desktop-auto-save)))

(declare-function eshell-save-some-history "ext:em-hist")
(defun persist-state--maybe-save-eshell ()
  "Save the Eshell history if active."
  (when (bound-and-true-p eshell-hist-mode)
    (eshell-save-some-history)))

(declare-function prescient--save "ext:prescient")
(defun persist-state--maybe-save-prescient ()
  "Save the prescient data if the package is active."
  (when (bound-and-true-p prescient-persist-mode)
    (prescient--save)))

(declare-function recentf-save-list "ext:recentf")
(defun persist-state--maybe-save-recentf ()
  "Save the list of recent files if the built-in recentf package is active."
  (when (bound-and-true-p recentf-mode)
    (recentf-save-list)))

(declare-function savehist-autosave "ext:savehist")
(defun persist-state--maybe-save-savehist ()
  "Save the history variables if the built-in savehist package is active."
  (when (bound-and-true-p savehist-mode)
    (savehist-autosave)))

(defun persist-state--maybe-save-places ()
  "Save places (positions in buffers) if the built-in saveplace package is active."
  (when (bound-and-true-p save-place-mode)
    (save-place-alist-to-file)))

(defvar persist-state-supported-packages-alist
  `((bookmark . (:function persist-state--maybe-save-bookmarks))
    (desktop . (:function persist-state--maybe-save-desktop))
    (em-hist . (:function persist-state--maybe-save-eshell
                          :label "Eshell history"))
    (prescient . (:function persist-state--maybe-save-prescient
                            :label "Prescient.el"
                            :url "https://github.com/radian-software/prescient.el"))
    (recentf . (:function persist-state--maybe-save-recentf))
    (savehist . (:function persist-state--maybe-save-savehist))
    (saveplace . (:function persist-state--maybe-save-places)))
  "A list of packages supported by persist-state.

Each package is a cons cell with the package name and a plist with:
- :function (mandatory): function to call to save state;
- :label (optional): a readable package name (for the README);
- :url (optional): URL to the package.")

(defcustom persist-state-saving-functions
  (mapcar (lambda (pkg) (plist-get pkg :function))
          (map-values persist-state-supported-packages-alist))
  "A list of functions that should be executed as part of saving state."
  :type '(repeat function)
  :group 'persist-state)

(defun persist-state--regularly-run-on-idle (f &rest args)
  "Run function F with ARGS at the configured interval (plus some idle time)."
  (run-with-timer persist-state-save-interval
                  persist-state-save-interval
                  (lambda ()
                    (run-with-idle-timer persist-state-wait-idle nil
                                         (lambda ()
                                           (apply f args))))))

(defun persist-state--save-state ()
  "Save state but only when the user was active recently."
  (let ((idle-time (float-time (or (current-idle-time) 0))))
    (when (< idle-time persist-state-save-interval)
      (mapc (lambda (f)
              (with-timeout (5 ; seconds
                             (message "persist-state: %s timed out"
                                      (symbol-name f)))
                (funcall f)))
            persist-state-saving-functions))))

(defun persist-state--enable ()
  "Start saving the Emacs state at the configured interval."

  ;; only start the timer once
  (when (null (timerp persist-state--save-state-timer))
    (setq persist-state--save-state-timer
          (persist-state--regularly-run-on-idle #'persist-state--save-state))))

(defun persist-state--disable ()
  "Stop saving the Emacs state."
  (when (timerp persist-state--save-state-timer)
    (cancel-timer persist-state--save-state-timer)))

;;;###autoload
(define-minor-mode persist-state-mode
  "Create minor mode to enable/disable saving state."
  :global t
  :lighter " Persist"

  (cond
   (persist-state-mode
    (persist-state--enable))
   (t
    (persist-state--disable))))

(provide 'persist-state)

;;; persist-state.el ends here
