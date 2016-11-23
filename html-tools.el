;;; html-tools.el ----- minor mode helper for processing/tagging html/text
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; Copyright 2016 Luis Miguel Castañeda Navas
;; License: GNU General Public License >= 3
;; Distribution: This file is not part of Emacs nor web-mode

;;==============================================================================
;; html-tools was sponsored initially by IAC http://iac.org.es/
;;==============================================================================

;; ad hoc functions to reformat existing html/text quickly using web-mode
;;

;;; Code:

;(require 'web-mode)

;; VARIABLES    -------------------------------------------------------

(defvar html-tools-words
  '("strong" "small" "em" ))

(defvar html-tools-paragraphs
  '("h1" "h2" "h3" "h4" "h5" "h6" "p"))

(defvar html-tools-lists
  '("ol" "ul"))

(defvar html-tools-containers
  '("body" "article" "aside" "main" "header" "footer" "section"))


(defun html-tools/bound-paragraph()
  ""
  (beginning-of-line)
  (mark-end-of-sentence 1)
  (save-excursion
	(narrow-to-region (region-beginning) (region-end))
	;; Trim any whitespace at the beggining/end of paragraph
	(save-restriction
	  (goto-char (point-min))
	  (when (re-search-forward "^[[:blank:]]+" nil t)
		(replace-match "" nil nil))
	  (goto-char (point-min))
	  (when (re-search-forward "[[:blank:]]*$" nil t)
		(replace-match "" nil nil))
	  (goto-char (point-max))
	  (newline)))
  (widen)
  (setq deactivate-mark nil))

(defun html-tools/bound-word()
  ""
  (let (bounds)
	(setq bounds (bounds-of-thing-at-point 'word))
	(goto-char (car bounds))
	(push-mark-command nil)
	(goto-char (cdr bounds))))


(defun html-tools/dwim-tag (tag)
  "Find the bounds of element and apply element warp.
TAG is the tag to add/replace."
  (let* (bounds parent)
	(cond ((eq (region-active-p) nil)
										; No está la región activa
		   (when (member tag html-tools-paragraphs)				    ; Es tag de párrafo
			 (while (not (or
						  (member (web-mode-element-tag-name) html-tools-paragraphs)
						  (member (web-mode-element-tag-name) html-tools-containers)))
			   (web-mode-element-parent))

			 (cond ((member (web-mode-element-tag-name) html-tools-containers)
					(progn
					  (message "envolver")
					  (goto-char elem_pos)
					  (html-tools/bound-paragraph)
					  (web-mode-element-wrap tag)))

				   ((member (web-mode-element-tag-name) html-tools-paragraphs)
					(progn
					  (message "sustituir")
					  (web-mode-element-rename tag)))
				   )
			 ) ; when

		   (when (member tag html-tools-words)	         ; Es una tag para palabras
			 (html-tools/bound-word)
			 (web-mode-element-wrap tag)))

		  ((eq (region-active-p) t)
										                 ; Está la región activa
		   (web-mode-element-wrap tag))					 ; Envolvemos
		  ) ;cond
	) ;let
  ) ;defun

(defun html-tools/find-node()
  ""
  (interactive)
  (let ((elem_pos (point)) (action "") (tag    "h7") (pos))

	(while (not (or
				 (member (web-mode-element-tag-name) html-tools-paragraphs)
				 (member (web-mode-element-tag-name) html-tools-containers)))

	  (web-mode-element-parent)

	  (cond ((member (web-mode-element-tag-name) html-tools-containers)
			 (setq action "envolver"))
			((member (web-mode-element-tag-name) html-tools-paragraphs)
			 (setq action "sustituir")))
	  )									;while

	(message action)

	(cond ((string= action "envolver")
		   (message "envolver")
		   (goto-char elem_pos)
		   (html-tools/bound-paragraph)
		   (web-mode-element-wrap tag))

		  ((string= action "sustituir")
		   (message "sustituir")
		   (web-mode-element-rename tag)
		   ;; excursion copied from merge-tag
		   ;; (save-excursion
		   ;; 	 (when (and (> (length tag) 0)
		   ;; 				(web-mode-element-beginning)
		   ;; 				(looking-at "<\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)?\\)"))
		   ;; 	   (setq pos (point))
		   ;; 	   (unless (web-mode-element-is-void)
		   ;; 		 (save-match-data
		   ;; 		   (web-mode-tag-match)
		   ;; 		   (if (looking-at "</[ ]*\\([[:alnum:]]+\\(:?[-][[:alpha:]]+\\)?\\)")
		   ;; 			   (replace-match (concat "</" tag))
		   ;; 			 )))
		   ;; 	   (goto-char pos)
		   ;; 	   (replace-match (concat "<" tag))
		   ;; 	   ))
		   )
		  )
	)
  )

;; Line breaks           ---------------------------------------------------------------------------------

(defun html-tools/mk-br()         "Insert <br> at point" (interactive) (insert "<br>") (newline))

;; word - region tags    ---------------------------------------------------------------------------------

(defun html-tools/mk-strong()	  "Warp current element with <strong>"     (interactive) (html-tools/dwim-tag "strong"))
(defun html-tools/mk-small()	  "Warp current element with <small>"      (interactive) (html-tools/dwim-tag "small"))
(defun html-tools/mk-em()		  "Warp current element with <em>"         (interactive) (html-tools/dwim-tag "em"))
(defun html-tools/mk-blockquote() "Warp current element with <blockquote>" (interactive) (web-mode-element-wrap "blockquote"))

;; Paragraphs / headings ---------------------------------------------------------------------------------

(defun html-tools/mk-paragraphs() "Convert region to paragraph." (interactive) (html-tools/dwim-tag "p"))
(defun html-tools/mk-h1()		  "Convert region to heading 1." (interactive) (html-tools/dwim-tag "h1"))
(defun html-tools/mk-h2()		  "Convert region to heading 2." (interactive) (html-tools/dwim-tag "h2"))
(defun html-tools/mk-h3()		  "Convert region to heading 3." (interactive) (html-tools/dwim-tag "h3"))
(defun html-tools/mk-h4()		  "Convert region to heading 4." (interactive) (html-tools/dwim-tag "h4"))
(defun html-tools/mk-h5()		  "Convert region to heading 5." (interactive) (html-tools/dwim-tag "h5"))
(defun html-tools/mk-h6()		  "Convert region to heading 6." (interactive) (html-tools/dwim-tag "h6"))

(defun html-tools/linkify()
  "Region to link"
  (interactive)
  (let ((inicio (region-beginning)) (fin (region-end)))
	(copy-to-register 'i  inicio fin)
	(narrow-to-region inicio fin)
	(save-excursion
	  (save-restriction
		(goto-char (point-min))
		(insert (concat "<a href=\"" (get-register 'i) "\" target=\"_blank\">"))
		(goto-char (point-max))
		(insert "</a>")))
	(widen)))

;; Lists    ---------------------------------------------------------------------------------

(defun html-tools/make-ul()
  ""
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (save-restriction
	(goto-char (point-min))
	(save-excursion
	  (while (re-search-forward "^[•–-#][[:blank:]]+" nil t)
		(replace-match "" nil nil))
	  (goto-char (point-min))
	  (while (re-search-forward "^\\(.+$\\)" nil t)
		(replace-match  "<li>\\1</li>" nil nil))
	  (goto-char (point-min))
	  (insert (concat "<ul>" (newline)))
	  (goto-char (point-max))
	  (insert "</ul>" (newline))))
  (widen)
  (web-mode-buffer-indent))

(defun html-tools/make-ol()
  ""
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (save-restriction
	(goto-char (point-min))
	(save-excursion
	  (while (re-search-forward "^[0-9]+[\.\-]+[[:blank:]]+" nil t)
		(replace-match "" nil nil))
	  (goto-char (point-min))
	  (while (re-search-forward "^\\(.+$\\)" nil t)
		(replace-match  "<li>\\1</li>" nil nil))
	  (goto-char (point-min))
	  (insert (concat "<ol>" (newline)))
	  (goto-char (point-max))
	  (insert (concat "</ol>" (newline)))))
  (widen)
  (web-mode-buffer-indent))

;; Footnotes    ---------------------------------------------------------------------------------

(defun html-tools/make-footnote-reference()
  ""
  (interactive)
  (let ((inicio (region-beginning)) (fin (region-end)))
	(copy-to-register 'i inicio fin)
	(narrow-to-region inicio fin)
	(save-restriction
	  (goto-char (point-min))
	  (save-excursion
		(insert (concat "<sup><a href=\"#ft" (get-register 'i) "\" id=\"" (get-register 'i) "\">"))
		(goto-char (point-max))
		(insert "</a></sup>")))
	(widen)))

(defun html-tools/make-footnote()
  ""
  (interactive)
  (let ((inicio (region-beginning)) (fin (region-end)))
	(copy-to-register 'i inicio fin)
	(narrow-to-region inicio fin)
	(save-excursion
	  (goto-char (point-min))
	  (save-restriction
		(insert ((concat "<p id=\"ft" (get-register 'i) "\"><a href=\"#" (get-register 'i) "\">"))
				(goto-char (point-max))
				(insert "</a>")))
	  (widen)
	  (end-of-line)
	  (insert "</p>"))))

;; Minor mode definition    ---------------------------------------------------------------------------------

(define-minor-mode html-tools-mode
  "Easy formatting of html code."
  :lighter " html"
  :keymap (let ((html-tools-map (make-sparse-keymap)))
			;; line breaks           -------------------------------------------------------
			(define-key html-tools-map [S-return]  'html-tools/mk-br)
			;; word - region tags    -------------------------------------------------------
			(define-key html-tools-map (kbd "H-b") 'html-tools/mk-strong)
			(define-key html-tools-map (kbd "H-s") 'html-tools/mk-small)
			(define-key html-tools-map (kbd "H-i") 'html-tools/mk-em)
			(define-key html-tools-map (kbd "H-B") 'html-tools/mk-blockquote)
			;; paragraphs - headings  -------------------------------------------------------
			(define-key html-tools-map (kbd "H-p") 'html-tools/mk-paragraphs)
			(define-key html-tools-map (kbd "H-1") 'html-tools/mk-h1)
			(define-key html-tools-map (kbd "H-2") 'html-tools/mk-h2)
			(define-key html-tools-map (kbd "H-3") 'html-tools/mk-h3)
			(define-key html-tools-map (kbd "H-4") 'html-tools/mk-h4)
			(define-key html-tools-map (kbd "H-5") 'html-tools/mk-h5)
			(define-key html-tools-map (kbd "H-6") 'html-tools/mk-h6)
			;; word - region tags    -------------------------------------------------------
			;; word - region tags    -------------------------------------------------------

			(define-key html-tools-map (kbd "H-u") 'html-tools/make-ul)
			(define-key html-tools-map (kbd "H-o") 'html-tools/make-ol)

			(define-key html-tools-map (kbd "H-a") 'html-tools/linkify)

			(define-key html-tools-map (kbd "H-f r") 'html-tools/make-footnote-reference)
			(define-key html-tools-map (kbd "H-f f") 'html-tools/make-footnote)

			html-tools-map))

(add-hook 'web-mode-hook 'html-tools-mode)
(provide 'html-tools)

;;; html-tools ends here