;;; html-tools.el ----- minor mode helper for processing/tagging html/text
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; Copyright 2016-2018 Luis Miguel Castañeda Navas
;; License: GNU General Public License >= 3
;; Distribution: This file is not part of Emacs nor web-mode

;;==============================================================================
;; html-tools was sponsored initially by IAC http://iac.org.es/
;;==============================================================================

;; ad hoc functions to reformat existing html/text quickly using web-mode
;;

;;; Code:

(require 'web-mode)

;; VARIABLES    -------------------------------------------------------

(defvar html-tools-words
  '("strong" "small" "em" ))

(defvar html-tools-paragraphs
  '("h1" "h2" "h3" "h4" "h5" "h6" "p"))

(defvar html-tools-lists
  '("ol" "ul"))

(defvar html-tools-containers
  '("body" "article" "aside" "main" "header" "footer" "section" "div" "blockquote"))
(defvar html-tools-elem-pos       nil)
(defvar html-tools-elem-beg       nil)
(defvar html-tools-elem-end       nil)
(defvar html-tools-parent-tag     nil)
(defvar html-tools-parent-element nil)
(defvar html-tools-current-tag    nil)

;; CORE UTILITIES ------------------------------------------------------

(defun html-tools/bound-paragraph()
	"Shrink region to paragraph content.
No spaces, newlines, etc."
	(interactive)
	(mark-paragraph)
	(narrow-to-region (region-beginning)(region-end))
	(save-restriction
		(html-tools/first-of-it)

		(setq html-tools-elem-beg (point)
		      html-tools-current-tag (web-mode-element-tag-name))

		(push-mark (point) t t)
		(html-tools/last-of-it)
		(setq html-tools-elem-end (point))
		))

(defun html-tools/bound-word()
  "Word at point to region."
	(interactive)
  (let (bounds)
		(setq bounds (bounds-of-thing-at-point 'word))
		(goto-char (car bounds))
		(push-mark nil t t)
		(goto-char (cdr bounds))))

(defun html-tools/first-of-it()
	"Find first narrowed buffer char."
	;;(when (not (buffer-narrowed-p)) )
	(goto-char (point-min))
	(re-search-forward "[^[:alnum:][:punct:]]+" nil t))

(defun html-tools/last-of-it()
	"Find last narrowed buffer char."
	(goto-char (point-max))
	(re-search-backward "[^[:alnum:][:punct:]]+" nil t))

(defun html-tools/select-target()
	"Return current region content or find next word and return it making a region."
	(let (content)
		(if (region-active-p)
				(setq content  (buffer-substring (region-beginning)(region-end)))

			(if (thing-at-point 'whitespace)	         ; NO WHITESPACES
					(progn (forward-word) (backward-word))) ; place cursor at beginning of next word

			(setq content
			 			(progn (html-tools/bound-word)
			 						 (buffer-substring (region-beginning)(region-end)))))))


(defun html-tools/dwim-tag (tag)
  "Find the bounds of element and apply element warp.
TAG is the tag to add/replace."
  (let* ((elem_pos (point)) bounds parent)
		(cond ((eq (region-active-p) nil)	                 ; No está la región activa
					 (when (member tag html-tools-paragraphs)		 ; Es tag de párrafo
						 (while (not (or
													(member (web-mode-element-tag-name) html-tools-paragraphs)
													(member (web-mode-element-tag-name) html-tools-containers)))
							 (web-mode-element-parent))

						 (cond ((member (web-mode-element-tag-name) html-tools-containers)
										(progn				                 ; envolver
											(goto-char elem_pos)
											(html-tools/bound-paragraph)
											(web-mode-element-wrap tag)))

									 ((member (web-mode-element-tag-name) html-tools-paragraphs)
										(web-mode-element-rename tag)))
						 ) ; when

					 (when (member tag html-tools-words)	         ; Es una tag para palabras
						 (html-tools/bound-word)
						 (web-mode-element-wrap tag)))

					((eq (region-active-p) t)		                 ; Está la región activa
					 (web-mode-element-wrap tag))					 ; Envolvemos
					) ;cond
		) ;let
  ) ;defun
(defun html-tools/clean-vars()
	""
	(setq html-tools-elem-pos       nil
				html-tools-elem-beg       nil
				html-tools-elem-end       nil
				html-tools-parent-tag     nil
				html-tools-parent-element nil
				html-tools-current-tag    nil)
	)

(defun html-tools/set-references()
	""
	(setq html-tools-elem-pos (point)) 											 ; punto en el que se encuentra el cursor al iniciar el comando

	(html-tools/bound-paragraph)
	;; (save-excursion

	;; 	;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	;; 	(while (not (or
	;; 							 (member (web-mode-element-tag-name) html-tools-paragraphs)
	;; 							 (member (web-mode-element-tag-name) html-tools-containers)))
	;; 		(web-mode-element-parent)
	;; 		(message "2: %s" (web-mode-element-tag-name))
	;; 		)

	;; 	(setq html-tools-parent-element (web-mode-element-parent))
	;; 	(setq html-tools-parent-tag (web-mode-element-tag-name)))
	;; 	;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	;; (message "%s" html-tools-parent-element)
	;; (message "%s" html-tools-parent-tag)

	(widen)
	(deactivate-mark)
	(message "%s" html-tools-elem-pos)
	(message "%s" html-tools-elem-beg)
	(message "%s" html-tools-elem-end)
	)

(defun html-tools/get-parent( )
	""
	(save-excursion
		(while (not (or
								 (member (web-mode-element-tag-name) html-tools-paragraphs)
								 (member (web-mode-element-tag-name) html-tools-containers)))
			(web-mode-element-parent)
			(message "2: %s" (web-mode-element-tag-name))
			)
		(setq html-tools-parent-element (web-mode-element-parent))
		(setq html-tools-parent-tag (web-mode-element-tag-name))))


;; Line breaks           ---------------------------------------------------------------------------------

(defun html-tools/mk-br()         "Insert <br> at point." (interactive) (insert "<br>") (newline))

;; word - region tags    ---------------------------------------------------------------------------------

(defun html-tools/mk-strong()	"Warp current element with <strong>." (interactive) (html-tools/dwim-tag "strong"))
(defun html-tools/mk-small()	"Warp current element with <small>."  (interactive) (html-tools/dwim-tag "small"))
(defun html-tools/mk-em()			"Warp current element with <em>."     (interactive) (html-tools/dwim-tag "em"))

;; Paragraphs / headings ---------------------------------------------------------------------------------

(defun html-tools/mk-blockquote() "Warp current element with <blockquote>." (interactive) (web-mode-element-wrap "blockquote"))
(defun html-tools/mk-paragraphs() "Convert region to paragraph."            (interactive) (html-tools/dwim-tag "p"))

(defun html-tools/mk-h1()	"Convert region to heading 1." (interactive) (html-tools/dwim-tag "h1"))
(defun html-tools/mk-h2()	"Convert region to heading 2." (interactive) (html-tools/dwim-tag "h2"))
(defun html-tools/mk-h3()	"Convert region to heading 3." (interactive) (html-tools/dwim-tag "h3"))
(defun html-tools/mk-h4()	"Convert region to heading 4." (interactive) (html-tools/dwim-tag "h4"))
(defun html-tools/mk-h5()	"Convert region to heading 5." (interactive) (html-tools/dwim-tag "h5"))
(defun html-tools/mk-h6()	"Convert region to heading 6." (interactive) (html-tools/dwim-tag "h6"))

(defun html-tools/mk-a()
	"Make a link from region if active, next word if not."
	(interactive)
	(let ((content (html-tools/select-target))) ;TODO: content no puede tener espacios en blanco
		(save-excursion
			(when (region-active-p) (kill-region (region-beginning)(region-end))) ; it's suppossed that it's always active as this point
			(insert (concat " <a href=\"" content "\" target=\"_blank\">" content "</a>")))))

;; Lists    ---------------------------------------------------------------------------------

(defun html-tools/mk-ul() "Formats lines from active region to an unordered list." (interactive) (html-tools/make-list "ul"))
(defun html-tools/mk-ol() "Formats lines from active region to an ordered list."   (interactive) (html-tools/make-list "ol"))

(defun html-tools/make-list(tag)
  (if (not (region-active-p)) 			; it only works on a region
			(message "Lists can be only formatted from a region")
		(narrow-to-region (region-beginning) (region-end))
		(save-excursion
			(save-restriction
				(goto-char (point-min))
				(while (re-search-forward "^\\([0-9]+[\.\-]+\\)?[[:blank:]]+" nil t)
					(replace-match "" nil nil))
				(goto-char (point-min))
				(while (re-search-forward "^\\(.+$\\)" nil t)
					(replace-match  "<li>\\1</li>" nil nil))
				(mark-paragraph)
				(web-mode-element-wrap tag))
			(delete-matching-lines "^$" (point-min) (point-max)))
		(widen)
		(web-mode-buffer-indent)))


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
				(insert (concat "<p id=\"ft" (get-register 'i) "\"><a href=\"#" (get-register 'i) "\">"))
				(goto-char (point-max))
				(insert "</a>"))
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

						(define-key html-tools-map (kbd "H-u") 'html-tools/mk-ul)
						(define-key html-tools-map (kbd "H-o") 'html-tools/mk-ol)

						(define-key html-tools-map (kbd "H-a") 'html-tools/mk-a)

						(define-key html-tools-map (kbd "H-f r") 'html-tools/make-footnote-reference)
						(define-key html-tools-map (kbd "H-f f") 'html-tools/make-footnote)

						html-tools-map))

(add-hook 'web-mode-hook 'html-tools-mode)

(provide 'html-tools)
;;; html-tools.el ends here
