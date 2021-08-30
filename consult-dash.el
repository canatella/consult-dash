;;; use-package-dash-docs.el --- Adds :dash keyword to use-package macro -*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Damien Merenne

;; Author: Damien Merenne <dam@cosinux.org>
;; Created:  Sep 2018
;; Version: 0.1
;; Package-Requires: ((emacs "24.4")  (dash-docs "1.4.0") (consult "0.10"))
;; Keywords: convenience extensions tools
;; URL: https://github.com/dash-docs-el/dash-docs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides support for the :dash keyword, which is made available by
;; default by requiring `use-package'.

;;; Code:

(require 'consult)
(require 'dash-docs)

(defvar consult-dash-history-input nil "Input history used by `consult--read'.")

(defvar consult-dash-sink nil "The current consult sink.")

(defun consult-dash--search-generator ()
  "Generate an async search closure."
  (thread-first
      (consult--async-sink)
    (consult--async-refresh-immediate)
    (consult-dash--async-search (current-buffer))
    (consult--async-throttle)))

(defun consult-dash--async-search (next buffer)
  "Handle asynchronous action using callback NEXT in the specified BUFFER."
  (lambda (action)
    (pcase action
      ((pred stringp)
       (when-let (candidates (with-current-buffer buffer (dash-docs-search action)))
         (funcall next 'flush)
         (funcall next candidates)))
      (_ (funcall next action)))))

(defun consult-dash-candidates ()
  "Return the current candidates."
  (when consult-dash-sink (funcall consult-dash-sink nil)))

;;;###autoload
(defun consult-dash (&optional initial)
  "Query dash docsets.
INITIAL will be used as the initial input, if given."
  (interactive)
  (dash-docs-initialize-debugging-buffer)
  (dash-docs-create-buffer-connections)
  (dash-docs-create-common-connections)
  (setq consult-dash-sink (consult-dash--search-generator))
  (let* ((dash-docs-candidate-format (if (featurep 'marginalia) "%n" dash-docs-candidate-format))
         (result
          (consult--read consult-dash-sink :prompt "Documentation for: " :initial initial :history 'consult-dash-history-input :category 'dash-docs-item)))
    (when result
      (dash-docs-browse-url (cdr (assoc result (consult-dash-candidates)))))))

;;;###autoload
(defun consult-dash-at-point ()
  "Bring up a `consult-dash' search interface with symbol at point."
  (interactive)
  (consult-dash (substring-no-properties (or (thing-at-point 'symbol) ""))))

(with-eval-after-load "marginalia"
  (defun consult-dash--annotate (candidate)
    "Compute marginalia fields for CANDIDATE."
    (when-let ((metadata (assoc candidate (consult-dash-candidates))))
      (pcase-let ((`(,title ,docset
                            (,type ,name ,url))
                   metadata))
        (marginalia--fields
         (type :face 'marginalia-type :width 10)
         (docset :face 'marginalia-file-name)))))

  (add-to-list 'marginalia-annotator-registry '(dash-docs-item consult-dash--annotate)))

(provide 'consult-dash)
;;; consult-dash.el ends here
