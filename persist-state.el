;;; persist-state.el --- Persist variables regularly  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Bram Schoenmakers

;; Author: Bram Schoenmakers <me@bramschoenmokers.nl>
;; Maintainer: Bram Schoenmakers <me@bramschoenmokers.nl>
;; Created: 05 May 2023

;; Package-Version: 0.1
;; Keywords: convenience
;; URL: https://apps.bram85.nl/git/bram/emacs-persist-state

;; This file is not part of GNU Emacs.

;;; Commentary:

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

(defvar persist-state-supported-packages
  `((bookmark . (:function bookmark-save))
    (desktop . (:function desktop-save :args (desktop-path)))
    (em-hist . (:function eshell-save-some-history :label "Eshell history"))
    (recentf . (:function recentf-save-list))
    (savehist . (:function savehist-autosave)))
  "A list of packages supported by persist-state.

Each package is a cons cell with the package name and a plist with:
- :function (mandatory): function to call to save state;
- :args (optional): arguments to be passed to the function;
- :label (optional): a readable package name (for the README).")

(defun persist-state--regularly-run-on-idle (interval idle-seconds f &rest args)
  "Run function F with ARGS every INTERVAL seconds, plus IDLE-SECONDS."
  (run-with-timer interval interval
                  (lambda ()
                    (run-with-idle-timer idle-seconds nil
                                         (lambda ()
                                           (apply f args))))))

(defun persist-state--save-state ()
  "Save state but only when the user was active recently."
  (when (< (float-time (current-idle-time)) persist-state-save-interval)
    (mapc 'funcall persist-state-saving-functions)))

(defun persist-state--enable-packages ()
  "Enables all supported packages.

If a package has no argumunts (no `:args' attribute, the function
is added as-is, otherwise it's wrapped in a lambda performing an
`apply' call.)"
  (mapc (lambda (package)
          (with-eval-after-load (car package)
            (let ((attrs (cdr package)))
              (add-to-list 'persist-state-saving-functions
                           (if (plist-member attrs :args)
                               (lambda () (apply (plist-get attrs :function)
                                            (plist-get attrs :args)))
                             (plist-get attrs :function))))))
        persist-state-supported-packages))

;;;###autoload
(defun persist-state-enable ()
  "Start saving the Emacs state at the configured interval."
  (interactive)
  (persist-state--enable-packages)

  (setq persist-state--save-state-timer
        (persist-state--regularly-run-on-idle persist-state-save-interval
                                 persist-state-wait-idle
                                 #'persist-state--save-state)))

;;;###autoload
(defun persist-state-disable ()
  (interactive)
  "Stop saving the Emacs state."
  (when (timerp persist-state--save-state-timer)
    (cancel-timer persist-state--save-state-timer)))

(provide 'persist-state)

;;; persist-state.el ends here
