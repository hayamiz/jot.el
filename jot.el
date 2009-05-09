;;; jot.el --- Just jot it

;; Copyright (C) 2009  haya

;; Author: Yuto Hayamizu <y.hayamizu@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:


(require 'easy-mmode)
(require 'thingatpt)
(require 'cl)

(defgroup jot-mode nil
  "Jot mode"
  :group 'convenience
  :prefix "jot-")

(defvar jot-mode-map nil)

(defvar jot-dicts nil)

(defcustom jot-file-name ".jot-mode" ""
  :group 'jot-mode
  :type 'string)

(defcustom jot-file-unified nil ""
  :group 'jot-mode
  :type 'boolean)

(if jot-mode-map
    nil
  (setq jot-mode-map (make-sparse-keymap))
  (define-key jot-mode-map (kbd "M-j") 'jot-it)
    )

(define-minor-mode jot-mode
  :lighter " Jot"
  :group 'jot-mode
  :keymap jot-mode-map
  (progn t))

(defun jot-after-save-hook-function ()
  (let ((regex (concat (regexp-quote jot-file-name) "$"))
	(file-name (buffer-file-name (current-buffer))))
    (when (and file-name
	       (string-match regex file-name))
      (jot-parse-jot-file file-name))))

(defun jot-mode-maybe ()
  (if (not (minibufferp (current-buffer)))
      (progn
	(add-hook 'after-save-hook
		  'jot-after-save-hook-function)
	(jot-mode t))))

(define-global-minor-mode global-jot-mode
  jot-mode jot-mode-maybe
  :group 'jot-mode)

(defun jot-file-name (file-name)
  (format "%s/%s"
	  (directory-file-name 
	   (if jot-file-unified
	       (getenv "HOME")
	     (file-name-directory file-name)))
	  jot-file-name))

(defun jot-parse-jot-file (file-name)
  (let ((jotbuf (jot-buffer file-name))
	(dict (jot-get-dict file-name))
	(dict-entries nil))
    (save-excursion
      (set-buffer jotbuf)
      (beginning-of-buffer)
      (while (re-search-forward
	      "^\\*\\* \\(\\sw\\|\\s_\\)+\\s-*:"
	      nil t)
	(goto-char (match-beginning 0))
	(let (bol eol (curline (current-line)))
	  (beginning-of-line)(setq bol (point))
	  (end-of-line)(setq eol (point))
	  (let ((line-str (buffer-substring bol eol)))
	    (set-text-properties 0 (length line-str) nil line-str)
	    (setq dict-entries
		  (cons 
		   (jot-file-line-to-dict-entry
		    line-str curline)
		   dict-entries))))))
    (rplacd dict dict-entries)))

(defun jot-file-line-to-dict-entry (line lineno)
  (unless (string-match
	   "^\\*\\* \\(\\(\\sw\\|\\s_\\)+\\)\\s-*:" line)
    (error "Invalid argument"))
  (let ((sym (match-string 1 line)))
    (set-text-properties 0 (length sym) nil sym)
    `(,sym . ,lineno)))

(defun jot-read-it ()
  (let ((thing (thing-at-point 'symbol)))
    (remove-text-properties 0 (length thing)
			    '(face) thing)
    (let ((it (message
	       (read-string
		(format "Jot it(default: %s): " thing)
		"" nil thing))))
      (when (string-equal it "")
	(setq it thing))
      it)))

(defun jot-buffer (&optional file-name)
  (unless file-name
    (setq file-name (buffer-file-name (current-buffer))))
  (let ((buf (find-buffer-visiting
	      (expand-file-name (jot-file-name file-name)))))
    (if buf
	buf
      (setq buf (find-file-noselect (jot-file-name file-name)))
      (save-excursion
	(set-buffer buf)
	(jot-parse-jot-file file-name)))))

(defun jot-other-window ()
  (let ((curbuf (buffer-file-name (current-buffer))))
    (if curbuf
	(switch-to-buffer-other-window
	 (jot-buffer curbuf)))))

(defun jot-get-dict (file-name)
  (let ((jotname (jot-file-name file-name)))
    (let ((dict (assoc jotname jot-dicts)))
      (if dict
	  dict
	(let ((newcell `(,jotname . ())))
	  (add-to-list 'jot-dicts newcell)
	  newcell)
	))
    ))

(defun jot-it ()
  (interactive)
  (let ((it (jot-read-it))
	(place (expand-file-name
		(buffer-file-name
		 (current-buffer)))))
    (let (pt (dict-entry (assoc it (jot-get-dict place))))
      (unless dict-entry
	(save-excursion
	  (set-buffer (jot-buffer))
	  (end-of-buffer)
	  (unless (eq 0 (current-column))
	    (newline))
	  (newline)
	  (insert (format "** %s: " it))
	  (setq pt (point))
	  (newline)))
      (jot-other-window)
      (if dict-entry
	  (goto-line (cdr dict-entry))
	(goto-char pt))
      (message it)
      )))

(provide 'jot)
;;; jot.el ends here
