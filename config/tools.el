;;; config/tools.el -*- lexical-binding: t; -*-

;; Lookup
(setq +lookup-open-url-fn #'eww)

(after! org
;; Helm-Bibtex
;; Open PDF in Evince not Firefox
(setq helm-external-programs-associations '(("pdf" . (if (eq system-type 'gnu/linux) "open" "evince"))))
;; Helm-Bibtex config
(setq reftex-default-bibliography '("~/Dokumente/References/my_zotero_library.bib")
      org-ref-completion-library 'org-ref-ivy-cite
      org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
      org-ref-default-bibliography '("~/Dokumente/References/my_zotero_library.bib")
      org-ref-notes-function
      (lambda (thekey)
	(let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
	  (bibtex-completion-edit-notes
	   (list (car (org-ref-get-bibtex-key-and-file thekey))))))
      bibtex-completion-bibliography '("~/Dokumente/References/my_zotero_library.bib")
      bibtex-completion-pdf-field "file"  ; For Zotero, see .bib file
      bibtex-completion-notes-path "~/org/org-roam/" ; One org-file for per publications
      bibtex-completion-notes-template-multiple-files
      (concat
        "${title}\n"
        "* TODO Notes\n"
        ":PROPERTIES:\n"
        ":NOTER_DOCUMENT: ${file}\n"
        ":END:\n\n"
        )))
