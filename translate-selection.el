;;; translate-selection.el --- Translate selected word in tooltip -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/unhammer/translate-selection
;; Keywords: convenience, translation

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defcustom translate-selection-max-selection-length 150
  "Don't auto-translate selections longer than this.
Explicit calls to `translate-selection' (not by mouse drag) will
still translate longer selections."
  :type 'integer
  :group 'translate-selection)

(defvar translate-selection-prev-selection nil
  "The previously translated selection.")

(defun translate-selection (&optional max-length)
  "Translate selected region.
If MAX-LENGTH is non-nil, don't translate selections longer than this."
  (interactive)
  (let* ((selection (buffer-substring-no-properties (region-beginning) (region-end)))
         (command (if (equal selection translate-selection-prev-selection)
                      '("trans")
                    '("trans" "-brief")))
         (len (length selection)))
    (when (and (> len 0)
               (<= len (or max-length
                           most-positive-fixnum)))
      (tooltip-hide)
      (let ((p (make-process :name "translate"
                             :buffer "*translate*"
                             :command command
                             :filter (lambda (_ s)
                                       (tooltip-show
                                        (format "%s → %s"
                                                selection
                                                (string-trim s)))))))
        (setq translate-selection-prev-selection selection)
        (process-send-string p selection)
        (process-send-string p "\n")
        (process-send-eof p)))))

(defun translate-selection-mouse (event)
  "Translate region after mouse drag (if it seems short but not empty).
EVENT passed to `mouse-drag-region'."
  (interactive "e")
  (funcall 'mouse-set-region event)
  (translate-selection translate-selection-max-selection-length))

(defcustom translate-selection-max-gap-context 100
  "When making gap-filling translations, limit context to this on each side."
  :type 'integer
  :group 'translate-selection)

(defun translate-selection-gapped-sentence (point)
  "Create a gapped sentence with word at POINT as the gap.
Sentence around word is the gap context, up to
`translate-selection-max-gap-context' on either side."
  (save-excursion
    (goto-char point)
    (when-let* ((sentence (bounds-of-thing-at-point 'sentence))
                (word (bounds-of-thing-at-point 'word))
                (word-beg (car word))
                (word-end (cdr word))
                (sent-beg (max (car sentence)
                               (- word-beg translate-selection-max-gap-context)))
                (sent-end (min (cdr sentence)
                               (+ word-end translate-selection-max-gap-context))))
      (cons
       (buffer-substring-no-properties word-beg word-end)
       (concat (buffer-substring-no-properties sent-beg
                                               word-beg)
               " ____________ "
               (buffer-substring-no-properties word-end
                                               sent-end))))))

(defun translate-selection-gapped ()
  "Translate sentence around point, with word at point turned into a gap."
  (interactive)
  (let* ((gapped (translate-selection-gapped-sentence (point)))
         (target  (car gapped))
         (context (cdr gapped))
         (result ""))
    (if gapped
        (let ((p (make-process :name "translate"
                               :buffer "*translate*"
                               :command '("trans" "-brief")
                               :filter (lambda (_ s)
                                         (setq result (concat result
                                                              " "
                                                              (string-trim s)))
                                         (tooltip-show
                                          (replace-regexp-in-string
                                           "__+"
                                           (concat "[" target "]")
                                           result))))))
          (process-send-string p context)
          (process-send-string p "\n")
          (process-send-eof p))
      (message "Failed to find word/sentence at point"))))

(define-minor-mode translate-selection-mode
  "Toggle Translate Selection mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Translate Selection mode is enabled, selecting a word with
the mouse will send it through the \"trans\" command."
 :init-value nil
 :lighter "📚"
 :keymap '(([double-down-mouse-1] . translate-selection-mouse)
           ([triple-down-mouse-1] . translate-selection-mouse)
           ([drag-mouse-1] . translate-selection-mouse)
           ([f7] . translate-selection)
           ([f8] . translate-selection-gapped)))


(provide 'translate-selection)
;;; translate-selection.el ends here
