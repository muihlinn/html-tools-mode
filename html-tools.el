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

;;; Code:

(require 'web-mode)

;; VARIABLES    -------------------------------------------------------

(defvar html-tools-containers
  '("body" "article" "aside" "main" "header" "footer" "section" "div" ))

(defvar html-tools-br  "br")

(defvar html-tools-strong "strong")
(defvar html-tools-em     "em")
(defvar html-tools-small  "small")

(defvar html-tools-words (list html-tools-strong html-tools-small html-tools-em))

(defvar html-tools-h1 "h1" "Heading level 1." )
(defvar html-tools-h2 "h2" "Heading level 2." )
(defvar html-tools-h3 "h3" "Heading level 3." )
(defvar html-tools-h4 "h4" "Heading level 4." )
(defvar html-tools-h5 "h5" "Heading level 5." )
(defvar html-tools-h6 "h6" "Heading level 6." )

(defvar html-tools-p      "p")
(defvar html-tools-blockquote "blockquote")

(defvar html-tools-paragraphs (list html-tools-h1
																		html-tools-h2 html-tools-h3
																		html-tools-h4 html-tools-h5 html-tools-h6
																		html-tools-p html-tools-blockquote))

(defvar html-tools-ol "ol")
(defvar html-tools-ul "ul")
(defvar html-tools-li "li")

(defvar html-tools-lists (list html-tools-ol html-tools-ul))

(defvar footnotes-section-regexp "section id=\"footnotes")

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
			 						 (buffer-substring (region-beginning)(region-end)))))
		;(eval content)

		))


(defun html-tools/dwim-tag (tag)
	"Find the bounds of element and apply element warp.
TAG is the tag to add/replace."
	(let ( bounds )

		;; when region is active, only wrapping is needed
		(when (region-active-p) (web-mode-element-wrap tag))

		(when (not (region-active-p))                     ;;  check if region is NOT active
			;; (message "%s" "no hay región activa")

			(when (member tag html-tools-words)	            ;; Es una tag para palabras
				(html-tools/bound-word)
				(web-mode-element-wrap tag))

			(when (member tag html-tools-paragraphs)        ;; When tag is a paragraph tag
				;; (message "1: %s" "procesando tag de párrafo")
				(html-tools/set-references)				            ;; set element references
				(goto-char html-tools-elem-beg)
				;; (read-from-minibuffer "...")

				(cond ((not (web-mode-element-tag-name))
							 (progn
								 (push-mark html-tools-elem-beg t t)
								 (goto-char html-tools-elem-end)
								 (web-mode-element-wrap tag)
								 (when (region-active-p) (push-mark nil t nil))
								 ))

							((web-mode-element-tag-name)
							 (progn
								 (message "%s" (web-mode-element-tag-name))
								 (goto-char html-tools-elem-beg)
								 (web-mode-element-rename tag))
							 ))))
		(html-tools/clean-vars)))

;; 			 (if (and (member html-tools-parent-tag html-tools-containers)
;; 								(member (web-mode-element-tag-name elem) html-tools-paragraphs))
;; 					 (progn
;; 						 (message "3: %s" parent)
;; 				 (progn                                      ;; Wrap element next to or around point with the paragraph tag
;; 					 (goto-char elem_pos)
;; 					 (html-tools/bound-paragraph)
;; 					 (message "4: %s" html-tools-current-tag)
;; 					 (read-from-minibuffer "... ")
;; 					 ;; misaligned tags
;; 					 ;; ----------------------------------------------------
;; 					 ;; IF paragraph starts with a tag, wrap tags are alone,
;; 					 ;; unlike inline when not. why?
;; 					 (when (buffer-narrowed-p)
;; 						 (indent-region (point-min)(point-max))
;; 						 (goto-char (point-max))
;; 						 (widen)))
;; 				 )) ; when
;; 		 ;; Wrap element next to or around point with the word tag
;; 		))) ;defun

(defun html-tools/clean-vars()
	"Clear vars."
	(setq html-tools-elem-pos       nil
				html-tools-elem-beg       nil
				html-tools-elem-end       nil
				html-tools-parent-tag     nil
				html-tools-parent-element nil
				html-tools-current-tag    nil)
	)

(defun html-tools/set-references()
	"Set references from current element."
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
	"TODO:."
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

(define-skeleton html-tools/break-line "Skeleton for a line break." nil "<"html-tools-br">\n")
(defun html-tools/mk-br()     "Insert <br> at point." (interactive) (html-tools/break-line))

;; word - region tags    ---------------------------------------------------------------------------------

(defun html-tools/mk-strong()	"Warp current element with <strong>." (interactive) (html-tools/dwim-tag html-tools-strong))
(defun html-tools/mk-small()	"Warp current element with <small>."  (interactive) (html-tools/dwim-tag html-tools-small))
(defun html-tools/mk-em()			"Warp current element with <em>."     (interactive) (html-tools/dwim-tag html-tools-em))

;; Paragraphs / headings ---------------------------------------------------------------------------------

(defun html-tools/mk-blockquote() "Warp current element with <blockquote>." (interactive) (web-mode-element-wrap html-tools-blockquote))
(defun html-tools/mk-paragraphs() "Convert region to paragraph."            (interactive) (html-tools/dwim-tag html-tools-p))

(defun html-tools/mk-h1()	"Convert region to heading 1." (interactive) (html-tools/dwim-tag html-tools-h1))
(defun html-tools/mk-h2()	"Convert region to heading 2." (interactive) (html-tools/dwim-tag html-tools-h2))
(defun html-tools/mk-h3()	"Convert region to heading 3." (interactive) (html-tools/dwim-tag html-tools-h3))
(defun html-tools/mk-h4()	"Convert region to heading 4." (interactive) (html-tools/dwim-tag html-tools-h4))
(defun html-tools/mk-h5()	"Convert region to heading 5." (interactive) (html-tools/dwim-tag html-tools-h5))
(defun html-tools/mk-h6()	"Convert region to heading 6." (interactive) (html-tools/dwim-tag html-tools-h6))

;; Links                 ---------------------------------------------------------------------------------

(define-skeleton html-tools/link
	"Skeleton for a link tag."
	nil
	'(setq v1 href v2 content)
	"<a href=\"" - @ v1 "\" target=\""@"_blank\">" @ v2"</a> ")

(defun html-tools/mk-a()
	"Make a link from region if active, next word if not."
	(interactive)
	(let ( href content (data (html-tools/select-target)))
		(when (region-active-p) (kill-region (region-beginning)(region-end))) ; it's suppossed that it's always active as this point

		;; TODO: procesar data para separar [si hubiera] urls y texto.

		(setq href "#" content data)
																				; (insert (concat " <a href=\"" content "\" target=\"_blank\">" content "</a>"))
		(html-tools/link)
		))


;; Taken from: https://www.emacswiki.org/emacs/SkeletonMode ---------------------------

(add-hook 'skeleton-end-hook 'skeleton-make-markers)

(defvar *skeleton-markers* nil
  "Markers for locations saved in skeleton-positions.")

(defun skeleton-make-markers ()
	"TODO: doc."
	(while *skeleton-markers*
    (set-marker (pop *skeleton-markers*) nil))
  (setq *skeleton-markers*
				(mapcar 'copy-marker (reverse skeleton-positions))))

(defun html-tools/skeleton-next-position (&optional reverse)
  "Jump to next position in skeleton.
REVERSE - Jump to previous position in skeleton"
  (interactive "P")
  (let* ((positions (mapcar 'marker-position *skeleton-markers*))
				 (positions (if reverse (reverse positions) positions))
				 (comp (if reverse '> '<))
				 pos)
    (when positions
      (if (catch 'break
						(while (setq pos (pop positions))
							(when (funcall comp (point) pos)
								(throw 'break t))))
					(goto-char pos)
				(goto-char (marker-position
										(car *skeleton-markers*)))))))

;; End of taken code :-)  -------------------------------------------------------------

;; Just because I can't bring myself to have a lambda() into keybinding

(defun html-tools/skeleton-prior-position()
	"Jump to previous position in skeleton."
	(interactive)
	(html-tools/skeleton-next-position t))



;; Lists    ---------------------------------------------------------------------------------

(defun html-tools/mk-ul() "Formats lines from active region to an unordered list." (interactive) (html-tools/make-list html-tools-ul))
(defun html-tools/mk-ol() "Formats lines from active region to an ordered list."   (interactive) (html-tools/make-list html-tools-ol))

;; TODO: revamp list creation

(defun html-tools/make-list(tag)
	"TODO:.
TAG."
	(unless (region-active-p) 			; it only works on a region
		(message "Lists can be only formatted from a region"))
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
	(web-mode-buffer-indent))

;; Footnotes    ---------------------------------------------------------------------------------

;; TODO: keep track of footnote number, if any

(define-skeleton html-tools/footnote-container
	"Skeleton for footnotes div."
	nil
	>"<section id=\"footnotes\">\n\n"
	> _
	>"\n\n"
	>"</section>\n"
	)

(define-skeleton html-tools/footnote-reference
	"Skeleton for footnote reference."
	nil
	'(setq v1 (int-to-string 1))
	"<sup><a href=\"#ft_" v1 "\">" v1 "</a></sup> "
	)

(define-skeleton html-tools/footnote-body
	"Skeleton for footnote body."
	nil
	'(setq v1 (int-to-string 1))
	>"<div id=\"ft_" v1 "\">\n\n"
	>"<span class=\"ft_index \">"v1"</span>\n"
	> _
	>"\n\n"
	>"</div>"
	)


;; TODO: should footnotes be inserted at the end of body, main, article ...?

(defun html-tools/insert-footnotes-container()
	"Insert footnotes container at the end of body."
	(interactive)
		;; find container parent which is semantically significant
		;; by now, just body

		;; TODO if element body doesn't exist, place it at the EOF

		(while (not (equal (web-mode-element-tag-name) "body"))
			(web-mode-element-parent))

		(web-mode-tag-match)							              ; goto just before its closing tag
		(html-tools/footnote-container)				          ; insert skeleton
		)


;; TODO: reindent, m-q

(defun html-tools/mk-footnote()
	"Insert footnote reference at point.
Requires working with a selection."

	(interactive)
	(let (next-footnote-pos footnote rb re)
		(save-excursion

			(if (region-active-p)
					(progn
						(setq rb (region-beginning)						   ; record region
									re (region-end))
						(setq footnote (buffer-substring rb re)) ; save region
						(kill-region rb re))

				(setq footnote ""))

			(html-tools/footnote-reference)							   ; insert skeleton

			;; goto last point inside section#footnotes
			;; insert footnote [from region].

			(setq next-footnote-pos (html-tools/find-footnotes-container))
			(message "%s" next-footnote-pos)
			(goto-char next-footnote-pos)
			(html-tools/footnote-body)
			(insert footnote))
		)
	)

;; TODO: debería poderse encontrar por algún método estándar, no buscando section id="footnotes"

(defun html-tools/find-footnotes-container()
	"Find footnotes container."
	(interactive)

	(if (re-search-forward footnotes-section-regexp nil t)
			(progn
				(web-mode-tag-match)									     ; goto end of tag
				(point))
		(progn									                       ; Si no existe contenedor de notas, crear
			(html-tools/insert-footnotes-container)
			(html-tools/find-footnotes-container))))

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

						;; move between skeleton positions

						(define-key html-tools-map (kbd "H-<next>")  'html-tools/skeleton-next-position)
						(define-key html-tools-map (kbd "H-<prior>") 'html-tools/skeleton-prior-position)

						html-tools-map))

(add-hook 'web-mode-hook 'html-tools-mode)

(provide 'html-tools)
;;; html-tools.el ends here
