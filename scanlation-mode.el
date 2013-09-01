;;; scanlation-mode.el --- Helper mode for producing manga translation scripts

;; Copyright (C) 2008  mathrick

;; Author: mathrick <mathrick@hatsumi>
;; Keywords: convenience, tools, editing, translation

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar scanlation-comment-start "//"
  "Plain comment starter string")

(defvar scanlation-directive-start "//!"
  "Directive comment starter string")

(defun scanlation-common-prefix (items)
  (let ((prefix "")
        (idx 0))
    (when items
      (flet ((char-at (string index)
                      (when (< index (length string))
                        (elt string index))))
        (while (and (char-at (car items) idx)
                    (every (lambda (item) 
                             (eql (downcase (char-at item idx)) 
                                  (downcase (char-at (car items) idx))))
                           items))
          (setq prefix (concat prefix (list (char-at (car items) idx))))
          (incf idx))))
    prefix))

(defun scanlation-make-completion (collection)
  "Make a completion function. Needed because the builtin
`completing-read' is case-sensitive."
  (lexical-let ((collection collection))
    (lambda (string pred all)
      (let ((result (remove-if-not (lambda (cand)
                                     (string-match-p (format "^%s" string) cand))
                                   collection)))
        (if all
            result
          (scanlation-common-prefix result))))))

(defvar scanlation-known-panel-names
  (scanlation-make-completion
   '("Title"
     "Extra"
     ))
  "Predefined other-than-numeric panel types.")

(defun scanlation-syntactic-face-function (parse)
  (save-excursion
    (let ((beg (elt parse 8)))
      (goto-char beg)
      (if (looking-at-p scanlation-directive-start)
        font-lock-keyword-face
        font-lock-comment-face))))

(defvar scanlation-mode-syntax-table
       (let ((st (make-syntax-table)))
         (modify-syntax-entry ?/ "< 12" st)
         (modify-syntax-entry ?\n ">   " st)
         st)
       "Syntax table used while in `text-mode'.")

(define-derived-mode scanlation-mode text-mode "Scanlation"
  "Major mode for editing scripts for scanlated manga."
  :syntax-table scanlation-mode-syntax-table
  (setq font-lock-defaults
          '(nil nil t nil nil (font-lock-syntactic-face-function . scanlation-syntactic-face-function)))
  (set (make-local-variable 'comment-start) "//")
  )

(defun scanlation-insert-directive (directive &rest args)
  (interactive "sInsert directive: ")
  (newline)
  (insert (format "%s %s" scanlation-directive-start directive))
  (mapcar (lambda (arg)
            (insert (format " %s" arg)))
          args))

(defun scanlation-find-directive (directive &optional backwards-p bound)
  (save-match-data
    (save-excursion
     (let ((case-fold-search t))
       (if (funcall (if backwards-p
                     'search-backward-regexp
                     'search-forward-regexp) 
                    (format "^%s %s" scanlation-directive-start directive)
                    bound t)
           (match-end 0)
         (or bound (point-min)))))))

(defun scanlation-next-panel-number ()
  (save-excursion
    (let ((bop (scanlation-find-directive "BEGIN PAGE" t))
          number)
      (while (and (not number)
                  (> (point) bop))
        (beginning-of-line)
        (message "panel @ %s" (goto-char (scanlation-find-directive "PANEL" t bop)))
        (save-match-data
         (when (looking-at "[[:space:]]*[0-9]+")
           (message "Found panel: %s" (string-to-number (match-string 0)))
           (setq number (1+ (string-to-number (match-string 0)))))))
      (or number 1))))

(defun scanlation-insert-panel (&optional prompt)
  "Insert a new panel. With argument, prompt for panel name to insert,
otherwise, automatically number the new panel."
  (interactive "P")
  (let ((panel (if prompt
                   (completing-read "Insert panel: "
                                    scanlation-known-panel-names)
                 (scanlation-next-panel-number))))
    (unless (save-excursion
              (beginning-of-line)
              (looking-at-p "^[[:space:]]*$"))
      (newline))
    (scanlation-insert-directive "PANEL" panel)
    (insert "\n")))

(provide 'scanlation-mode)
;;; scanlation-mode.el ends here
