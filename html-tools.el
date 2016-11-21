;;; html-tools.el ----- helping to mark html</p>
;;; Commentary:
;;; Code:

(require 'web-mode)

(defun html-tools/linkify()
	""
	(interactive)
	(let (inicio fin)
	  (setq inicio (region-beginning))
	  (setq fin    (region-end))
	  (copy-to-register 'i inicio fin)
	  (narrow-to-region inicio fin)
	  (save-restriction
		(goto-char (point-min))
		(insert "<a href=\"")
		(insert-register 'i t)
		(insert "\" target=\"_blank\">")
		(goto-char (point-max))
		(insert "</a>")
		)
	  (widen)))

  (defun html-tools/tight-paragraph()
	""
	(interactive)
	(backward-paragraph)
	(next-line)
	(set-mark-command (beginning-of-line))
	(forward-paragraph)
	(previous-line)
	(end-of-line)
	(setq deactivate-mark nil))

  (defun html-tools/make-br()
	""
	(interactive)
	(insert "<br>")
	(newline)
	)

  (defun html-tools/make-strong()
	""
	(interactive)
	(web-mode-element-wrap "strong")
										;(setq deactivate-mark nil)
	)

  (defun html-tools/make-small()
	""
	(interactive)
	(web-mode-element-wrap "small")
										;(setq deactivate-mark nil)
	)


  (defun html-tools/make-em()
	""
	(interactive)
	(web-mode-element-wrap "em")
										;(setq deactivate-mark nil)
	)

  (defun html-tools/make-blockquote()
	""
	(interactive)
	(web-mode-element-wrap "blockquote")
										;(setq deactivate-mark nil)
	)


  (defun html-tools/make-paragraphs()
	""
	(interactive)
	(html-tools/tight-paragraph)
	(web-mode-element-wrap "p"))

  (defun html-tools/make-h1()
	""
	(interactive)
	(html-tools/tight-paragraph)
	(web-mode-element-wrap "h1"))

  (defun html-tools/make-h2()
	""
	(interactive)
	(html-tools/tight-paragraph)
	(web-mode-element-wrap "h2"))

  (defun html-tools/make-h3()
	""
	(interactive)
	(html-tools/tight-paragraph)
	(web-mode-element-wrap "h3"))

  (defun html-tools/make-h4()
	""
	(interactive)
	(html-tools/tight-paragraph)
	(web-mode-element-wrap "h4"))

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
		(insert "<ul>")
		(newline)
		(goto-char (point-max))
		(insert "</ul>")
		(newline)
		))
	(widen)
	(web-mode-buffer-indent)


	)

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
		(insert "<ol>")
		(newline)
		(goto-char (point-max))
		(insert "</ol>")
		(newline)
		))
	(widen)
	(web-mode-buffer-indent)
	)


  (defun html-tools/make-footnote-reference()
	""
	(interactive)
	(let (inicio fin)
	  (setq inicio (region-beginning))
	  (setq fin    (region-end))
	  (copy-to-register 'i inicio fin)
	  (narrow-to-region inicio fin)
	  (save-restriction
		(goto-char (point-min))
		(save-excursion
		  (insert "<sup><a href=\"#ft")
		  (insert-register 'i t)
		  (insert "\" id=\"")
		  (insert-register 'i t)
		  (insert "\">")
		  (goto-char (point-max))
		  (insert "</a></sup>")))
	  (widen)))

  (defun html-tools/make-footnote()
	""
	(interactive)
	(let (inicio fin)
	  (setq inicio (region-beginning))
	  (setq fin    (region-end))
	  (copy-to-register 'i inicio fin)
	  (narrow-to-region inicio fin)
	  (save-restriction
		(goto-char (point-min))
		(save-excursion
		  (insert "<p id=\"ft")
		  (insert-register 'i t)
		  (insert "\"><a href=\"#")
		  (insert-register 'i t)
		  (insert "\">")
		  (goto-char (point-max))
		  (insert "</a>")))
	  (widen)
	  (end-of-line)
	  (insert "</p>")))


(define-minor-mode html-tools-mode
  "Easy formatting of html code."
  :lighter " html"
  :keymap (let ((html-tools-map (make-sparse-keymap)))
			(define-key html-tools-map (kbd "H-p") 'html-tools/make-paragraphs)
			(define-key html-tools-map (kbd "H-b") 'html-tools/make-strong)
			(define-key html-tools-map (kbd "H-i") 'html-tools/make-em)

			(define-key html-tools-map (kbd "H-u") 'html-tools/make-ul)
			(define-key html-tools-map (kbd "H-o") 'html-tools/make-ol)

			(define-key html-tools-map (kbd "H-B") 'html-tools/make-blockquote)
			(define-key html-tools-map (kbd "H-s") 'html-tools/make-small)
			(define-key html-tools-map [S-return]  'html-tools/make-br)
			(define-key html-tools-map (kbd "H-L") 'html-tools/linkify)

			(define-key html-tools-map (kbd "H-1") 'html-tools/make-h1)
			(define-key html-tools-map (kbd "H-2") 'html-tools/make-h2)
			(define-key html-tools-map (kbd "H-3") 'html-tools/make-h3)
			(define-key html-tools-map (kbd "H-4") 'html-tools/make-h4)

			(define-key html-tools-map (kbd "H-f r") 'html-tools/make-footnote-reference)
			(define-key html-tools-map (kbd "H-f f") 'html-tools/make-footnote)

			html-tools-map))

(add-hook 'web-mode-hook 'html-tools-mode)
(provide 'html-tools)

;;; html-tools ends here
