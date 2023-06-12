;;; persist-state.el --- Regularly persist bookmarks, history, recent files and more -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Bram Schoenmakers

;; Author: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Maintainer: Bram Schoenmakers <me@bramschoenmakers.nl>
;; Created: 05 May 2023
;; Package-Requires: ((emacs "28.2"))
;; Keywords: convenience
;; URL: https://codeberg.org/bram85/emacs-persist-state.git

;; This file is not part of GNU Emacs.

;; MIT License

;; Copyright (c) 2023 Bram Schoenmakers

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

;; This package saves your Emacs state regularly when idle for a brief
;; moment, to minimize disturbance of executing all save functions in
;; sequence. In case Emacs is idle for a longer time, no state is
;; saved.

;; See README.org for more details.

;;; Code:

(defgroup persist-state nil
  "Persist the Emacs state at regular intervals."
  :group 'convenience)

(defcustom persist-state-saving-functions nil
  "A list of functions that should be executed as part of saving state."
  :type '(repeat function)
  :group 'persist-state)

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

(defvar persist-state-supported-packages-alist
  `((bookmark . (:function (lambda () (when bookmark-save-flag
                                   (bookmark-save)))))

    (desktop . (:function (lambda () (when desktop-save-mode
                                  (desktop-save (car desktop-path) nil t)))))

    (em-hist . (:function eshell-save-some-history
                          :label "Eshell history"))

    (prescient . (:function (lambda () (when prescient-persist-mode
                                    (prescient--save)))
                            :label "Prescient.el"
                            :url "https://github.com/radian-software/prescient.el"))

    (recentf . (:function recentf-save-list))

    (savehist . (:function (lambda () (when savehist-mode
                                   (savehist-autosave))))))
  "A list of packages supported by persist-state.

Each package is a cons cell with the package name and a plist with:
- :function (mandatory): function to call to save state;
- :label (optional): a readable package name (for the README);
- :url (optional): URL to the package.")

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
  (when (< (float-time (current-idle-time)) persist-state-save-interval)
    (mapc #'funcall persist-state-saving-functions)))

(defun persist-state--enable-packages ()
  "Enables all supported packages.

If a package has no arguments (no `:args' attribute, the function
is added as-is, otherwise it's wrapped in a lambda performing an
`apply' call.)"
  (mapc (lambda (package)
          (with-eval-after-load (car package)
            (let ((attrs (cdr package)))
              (add-to-list 'persist-state-saving-functions
                           (plist-get attrs :function)))))
        persist-state-supported-packages-alist))

(defun persist-state--enable ()
  "Start saving the Emacs state at the configured interval."
  (persist-state--enable-packages)

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
