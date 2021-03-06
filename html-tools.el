;;; html-tools.el ----- minor mode helper for processing/tagging html/text
;;; -*- coding: utf-8 -*-

;;; Commentary:

;; Copyright 2016-2019 Luis Miguel Castañeda Navas
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

(defvar html-tools-link   "a")
(defvar html-tools-em     "em")
(defvar html-tools-strong "strong")
(defvar html-tools-small  "small")
(defvar html-tools-s			"s")
(defvar html-tools-cite		"cite")
(defvar html-tools-q			"q")
(defvar html-tools-dfn		"dfn")
(defvar html-tools-abbr		"abbr")
(defvar html-tools-data		"data")
(defvar html-tools-time		"time")
(defvar html-tools-code		"code")
(defvar html-tools-var		"var")
(defvar html-tools-samp		"samp")
(defvar html-tools-kbd		"kbd")
(defvar html-tools-sub		"sub")
(defvar html-tools-sup		"sup")
(defvar html-tools-i			"i")
(defvar html-tools-b			"b")
(defvar html-tools-u			"u")
(defvar html-tools-mark		"mark")
(defvar html-tools-ruby		"ruby")
(defvar html-tools-rt			"rt")
(defvar html-tools-rp			"rp")
(defvar html-tools-bdi		"bdi")
(defvar html-tools-bdo		"bdo")
(defvar html-tools-span		"span")
(defvar html-tools-br			"br")
(defvar html-tools-wbr		"wbr")

(defvar html-tools-words (list
													html-tools-link
													html-tools-em
													html-tools-strong
													html-tools-small
													html-tools-s
													html-tools-cite
													html-tools-q
													html-tools-dfn
													html-tools-abbr
													html-tools-data
													html-tools-time
													html-tools-code
													html-tools-var
													html-tools-samp
													html-tools-kbd
													html-tools-sub
													html-tools-sup
													html-tools-i
													html-tools-b
													html-tools-u
													html-tools-mark
													html-tools-ruby
													html-tools-rt
													html-tools-rp
													html-tools-bdi
													html-tools-bdo
													html-tools-span
													html-tools-br
													html-tools-wbr))

(defvar html-tools-inline html-tools-words)

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

;; TODO: unused
(define-skeleton html-tools/container
	"Skeleton for a container."
	nil
	'(setq v1 tag)
	>"\n<"tag">"_"</"tag">\n\n")


(defun html-tools/dwim-tag (tag)
	"Find the bounds of element and apply element warp.
TAG is the tag to add/replace."
	(let ( bounds )

		(setq html-tools-elem-pos (point))

		;; when region is active, only wrapping is needed
		(when (region-active-p) (web-mode-element-wrap tag))

		(when (not (region-active-p))                       ;  check if region is NOT active

			(when (member tag html-tools-words)	              ; it is a word tag
				(html-tools/bound-word)
				(web-mode-element-wrap tag))

			(when (member tag html-tools-paragraphs)          ; When tag is a paragraph tag
				(html-tools/get-valid-parent)				            ; find the inmediate parent which fits the bill

				(cond
				 ((not (web-mode-element-tag-name))             ; element do not have an enclosing tag
					(html-tools/tag-orphan-paragraph tag)
					)

				 ((not html-tools-parent-tag)                   ; it's an orphan element
					(if (and (web-mode-element-tag-name)
									 (not (member (web-mode-element-tag-name) html-tools-inline)))
							(web-mode-element-rename tag)             ; Orphan, but not anonymous
						(html-tools/tag-orphan-paragraph tag)))	    ; Orphan and anonymous


				 ((web-mode-element-tag-name)		                ; we're over a tag

					;; TODO: recheck how this works, unneeded recursion?.
					(if (member (web-mode-element-tag-name) html-tools-inline)
							(progn (html-tools/get-valid-parent)
										 (html-tools/dwim-tag tag))
						(web-mode-element-rename tag))))

				(goto-char html-tools-elem-pos)
				(html-tools/clean-vars)))))


(defun html-tools/tag-orphan-paragraph (tag)
	"Tag an orphan paragraph.
Work over automatic selections.  Not to be used directly.
TAG is the tag to be used."
	(html-tools/bound-paragraph)
	(push-mark (html-tools/first-of-it))
	(goto-char (html-tools/last-of-it))
	(web-mode-element-wrap tag)
	(widen)
	(when (region-active-p) (push-mark nil t nil))
	(goto-char html-tools-elem-pos)
	)

(defun html-tools/clean-vars()
	"Clear vars."
	(setq html-tools-elem-pos       nil
				html-tools-elem-beg       nil
				html-tools-elem-end       nil
				html-tools-parent-tag     nil
				html-tools-current-tag    nil)
	)


(defun html-tools/get-valid-parent( )
	"Look up element parent until find a paragraph or container which can be used."
	(interactive)
	(let (elem)
		(setq elem (web-mode-element-parent)
					html-tools-parent-tag (web-mode-element-tag-name))

		(cond ((eq elem nil)
					 (setq html-tools-parent-tag nil))

					((or (eq (web-mode-element-tag-name) nil)
							 (member (web-mode-element-tag-name) html-tools-inline))
					 (html-tools/get-valid-parent)))))


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

;;;###autoload
(define-minor-mode html-tools-mode
	"Easy formatting of html code."
	:lighter " htmlt"
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

;;;###autoload
(add-hook 'web-mode-hook 'html-tools-mode)

(provide 'html-tools)
;;; html-tools.el ends here
