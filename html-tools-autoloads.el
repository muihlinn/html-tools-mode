;;; autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "html-tools" "html-tools.el" (23577 15606 691551
;;;;;;  884000))
;;; Generated autoloads from html-tools.el

(autoload 'html-tools-mode "html-tools" "\
Easy formatting of html code.\n\n(fn &optional ARG)" t nil)

(add-hook 'web-mode-hook 'html-tools-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "html-tools" '(#("html-tools" 0 10 (fontified nil)) #("skeleton-make-markers" 0 21 (fontified nil)) #("*skeleton-markers*" 0 18 (fontified nil)) #("footnotes-section-regexp" 0 24 (fontified nil)))))

;;;***

(provide 'autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autoloads.el ends here
