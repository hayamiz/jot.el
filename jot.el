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

(defcustom jot-file-name ".jot" ""
  :group 'jot-mode
  :type 'string)

(defcustom jot-file-unified nil ""
  :group 'jot-mode
  :type 'boolean)

(if jot-mode-map
    nil
  (setq jot-mode-map (make-sparse-keymap))
  (define-key jot-mode-map (kbd "M-j") 'jot-it-just)
  (define-key jot-mode-map (kbd "M-k") 'jot-it-with-place)
    )

(define-minor-mode jot-mode
  "Jot mode"
  :lighter " Jot"
  :group 'jot-mode
  :keymap jot-mode-map
  (progn t))

(defun jot-after-save-hook-function ()
  (interactive)
  )

(defun jot-mode-maybe ()
  (if (not (minibufferp (current-buffer)))
      (progn
	(add-hook 'after-save-hook
		  'jot-after-save-hook-function)
	(jot-mode t))))

(define-global-minor-mode global-jot-mode
  jot-mode jot-mode-maybe
  :group 'jot-mode)

(defun jot-file-name (&optional file-name)
  (unless file-name
    (setq file-name (or (buffer-file-name (current-buffer))
			"~/dummy")))
  (setq file-name (expand-file-name file-name))
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
	      "^\\*\\* \\(\\sw\\|\\s_\\)+\\s-*\\(:\\|(\\)"
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

(defun jot-get-line-str (&optional lineno)
  (let (ret)
    (save-excursion
      (if lineno (goto-line lineno))
      (let (bof eof)
	(beginning-of-line) (setq bof (point))
	(end-of-line) (setq eof (point))
	(setq ret (buffer-substring bof eof))))
    ret))

(defun jot-get-line-obj (&optional lineno)
  (let ((linestr (jot-get-line-str lineno)))
    (let (keyword places-str (places nil))
      (if (null (string-match
		 "^\\*\\*\\s-+\\(\\(\\sw\\|\\s_\\)+\\)\\s-*\\(\\((.*)\\)?\\)\\s-*:" linestr))
	  nil
	(setq keyword (match-string 1 linestr))
	(setq places-str (match-string 3 linestr))
	(if (< (length places-str) 3)
	    (list keyword)
	  (setq places-str (substring places-str
				      1 (- (length places-str) 1)))
	  
	  ;; this codes still have a bug with pathnames with comma
	  (setq places (split-string places-str ","))
	  (cons keyword places))
	))))

(defun jot-line-obj-add-place (lineobj place)
  (let ((places (cdr lineobj)))
       (unless (member place places)
	 (setq places (cons place places))
	 (rplacd lineobj places)))
  lineobj)

(defun jot-line-obj-to-string (lineobj)
  (let ((keyword (car lineobj))
	(places (cdr lineobj)))
    (format "** %s%s: "
	    keyword
	    (if (null places) ""
	      (format "(%s)" (mapconcat 'identity places ","))))))

(defun jot-replace-line (lineobj &optional lineno)
  (save-excursion
    (if lineno
	(goto-line lineno))
    (let (bof eof)
      (beginning-of-line) (setq bol (point))
      (end-of-line) (setq eol (point))
      (delete-region bol eol))
    (insert (jot-line-obj-to-string lineobj))))

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
	(jot-parse-jot-file file-name))
      buf)))

(defun jot-other-window ()
  (switch-to-buffer-other-window
   (jot-buffer)))

(defun jot-it (&optional place)
  (interactive)
  (let ((it (jot-read-it))
	(current-file (expand-file-name
		       (or (buffer-file-name (current-buffer))
			   "~/dummy"))))
    (let (keyword-in-jotbuffer)
      (save-excursion
	(set-buffer (jot-buffer))
	(beginning-of-buffer)
	(setq keyword-in-jotbuffer
	      (re-search-forward
	       (format "^\\*\\* %s" (regexp-quote it))
	       nil t))
	;; (debug)
	(cond
	 ((null keyword-in-jotbuffer)
	  ;; there's no keyword `it' in the jot buffer
	  (end-of-buffer)
	  (unless (eq 0 (current-column))
	    (newline))
	  (insert
	   (format "\n** %s%s: \n" it
		   (if (null place)
		       ""
		     (format "(%s)" place)))))
	 (t
	  ;; 
	  (goto-char keyword-in-jotbuffer)
	  (when place
	    (let ((curlineobj (jot-get-line-obj)))
	      (jot-line-obj-add-place curlineobj place)
	      (jot-replace-line curlineobj))))
	 ))
      (jot-other-window)
      (beginning-of-buffer)
      (let ((pt (re-search-forward
		 (format "^\\*\\*\\s-+%s" (regexp-quote it)))))
	(unless pt (error (format "Cannot find keyword: %s" it)))
	(goto-char pt)
	(beginning-of-line)))
    (message it)
    ))

(defun jot-it-with-place ()
  (interactive)
  (let ((file-name (expand-file-name
		    (buffer-file-name (current-buffer))))
	(line-no (line-number-at-pos)))
    (if file-name
	(jot-it (format "%s:%d" file-name line-no))
      (jot-it))
  ))

(defun jot-it-just ()
  (interactive)
  (jot-it))

;;;
;;; jot-view-mode (major-mode)
;;;

(provide 'jot)
;;; jot.el ends here
