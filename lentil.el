;;; lentil.el --- a single pulse line highlight  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Gavin Massingham

;; Author: Gavin Massingham <gavmassingham+lentil@gmail.com>
;; Version: 1.00

;; Keywords: faces
;; URL: TBD

;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; 

;;; Code:

(require 'pulse)

(defgroup lentil nil "Options for controlling the behaviour of Lentil mode")

(defcustom lentil-inhibit-pulse-functions nil
  "An alist whose elements have the form (FUNCTION . SYMBOL).
FUNCTION should be a predicate function that returns t when the
conditions are met to inhibit or freeze the pulse.  SYMBOL should
be either `freeze' or `inhibit'."
  :type '(alist :key-type function :value-type (radio (const freeze)
  (const inhibit))))

(defcustom lentil-highlight-region-functions
  '((t . lentil--default-highlight-region))
  "An alist whose elements have the form (SYMBOL . FUNCTION).
SYMBOL should either be a major mode or the symbol t. FUNCTION
should be the name of a function that takes no arguments and
which sets the buffer-local variables `lentil-highlight-beginning'
and `lentil-highlight-end' to the beginning and end of the region
to be highlighted by Lentil.  If SYMBOL is t, this specifies a
default function to use."
  :type '(alist :key-type (choice (const t) (symbol :tag "Major Mode")) :value-type function))

(defface lentil-highlight
  '((default . (:inherit hl-line)))
  "Face used for the Lentil highlight pulse.
Defaults to hl-line.")

(defcustom lentil-pulse-iterations 12 "Number of iterations the pulse
goes through as it fades. Also see `lentil-pulse-delay'" :type 'integer)
(defcustom lentil-pulse-delay 0.045 "Amount of time between each
fading iteration of pulse. Also see `lentil-pulse-iterations'" :type 'number)
(defvar-local lentil--last-line 0 "Buffer-local variable that stores the
last line highlighted by Lentil.")
(defvar-local lentil-highlight-beginning 0 "Buffer-local variable
that stores the beginning of the region Lentil will highlight.")
(defvar-local lentil-highlight-end 0 "Buffer-local variable that stores
end of the region Lentil will highlight.")

(defun lentil--inhibit-pulse-maybe ()
  "Iterate through `lentil-inhibit-pulse-functions'.
Call each FUNCTION in turn.  If FUNCTION returns t, return the
associated SYMBOL.  Return nil if no FUNCTION returns t."
  (let ((val nil)
        (fns lentil-inhibit-pulse-functions))
    (while-let ((fns)
                (fn (caar fns)))
      (if (funcall fn)
          (setq val (cdar fns)
                fns nil)
        (setq fns (cdr fns))))
    val))

;;;###autoload
(defun lentil-pulse ()
  "Maybe highlight some region with a single pulse.
First call the function `lentil--inhibit-pulse-maybe'.  If that
function returns 'inhibit', then do not apply any highlighting.
If that function returns 'freeze', apply a static highlight
instead of a pulse.  By default, the region highlighted is the
current logical line, but this can be changed by specifying
alternative functions in `lentil-highlight-region-functions'.
Uses the face `lentil-highlight'."
  (let
      ((pulse-iterations lentil-pulse-iterations)
       (pulse-delay lentil-pulse-delay)
       (pulse-flag (not (eq (lentil--inhibit-pulse-maybe) 'freeze)))
       (inhibit (eq (lentil--inhibit-pulse-maybe) 'inhibit)))
    (or
     inhibit
     (and pulse-flag (= lentil--last-line
                        (line-number-at-pos (point))))
     (save-excursion
       (or (lentil--get-highlight-region)
           (lentil--default-highlight-region))
       (pulse-momentary-highlight-region
        lentil-highlight-beginning
        lentil-highlight-end
        'lentil-highlight)))))

(defun lentil--get-highlight-region ()
  "Call a function from `lentil-highlight-region-functions',\
if applicable.

The called function will set the beginning and end of the region
that Lentil will highlight.

Return the function called, or nil if no function was called."
  (if-let
      ((fn (or
             (alist-get major-mode lentil-highlight-region-functions)
             (alist-get t lentil-highlight-region-functions))))
    (or (funcall fn) fn)
    nil))

(defun lentil--default-highlight-region ()
  "Store the start and end positions of the current logical line.
These are stored in the local variables
`lentil-highlight-beginning' and `lentil-highlight-end'.

This function will be called if no default is specified in the
variable `lentil-highlight-region-functions'.

This function always returns t."
  (setq lentil-highlight-beginning (progn (forward-visible-line 0) (point))
        lentil-highlight-end (progn (forward-visible-line 1) (point)))
  t)

(defun lentil-on-outline-heading-p ()
  "An example function for `lentil-inhibit-pulse-functions'.
This function returns non-nil if point is on an Outline mode
heading."
  (and (fboundp #'outline-on-heading-p)
       (or (outline-on-heading-p)
           (save-excursion
             (and (not (or (bobp)(backward-char)))
                  (outline-invisible-p))))))
 
(defun lentil--store-current-line ()
  "Store the current line number."
  (setq lentil--last-line (line-number-at-pos (point) t)))

(defun lentil--add-or-remove-hooks ()
  "Add/remove hooks when enabling/disabling Lentil."
  (if (not lentil-mode)
      (progn (remove-hook 'pre-command-hook #'lentil--store-current-line t)
             (remove-hook 'post-command-hook #'lentil-pulse t))
    (progn (add-hook 'pre-command-hook #'lentil--store-current-line nil t)
           (add-hook 'post-command-hook #'lentil-pulse nil t))))

(define-minor-mode lentil-mode
  "Highlight the current line with a single pulse highlight.

Lentil is a simple buffer-local minor mode intended as an
alternative to `hl-line-mode'.  By default, the current logical
line is identified by a fading highlight.

Use the custom variable `lentil-inhibit-pulse-functions' to
define conditions under which a line should either not be
highlighted at all, or where a static, non-fading, highlight
should be used instead.

Use the custom variable `lentil-highlight-region-functions' to
specify alternative methods for defining the start and end of the
region to which the highlight is applied.

Lentil is so called because a lentil is a small pulse."

  :lighter nil

  (lentil--add-or-remove-hooks))

(provide 'lentil)

;;; lentil.el ends here
