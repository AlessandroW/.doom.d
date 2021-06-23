;;; config/tools.el -*- lexical-binding: t; -*-

;; Lookup
(setq +lookup-open-url-fn #'eww)

;; Helm-Bibtex
;; Open PDF in Evince not Firefox
(setq helm-external-programs-associations '(("pdf" . (if (eq system-type 'gnu/linux) "open" "evince"))))
(setq bibfile-path
      (if IS-MAC
          "~/references/my_zotero_library.bib"
          "~/Dokumente/References/my_zotero_library.bib"))
;; Helm-Bibtex config
(setq reftex-default-bibliography (list bibfile-path)
      org-ref-completion-library 'org-ref-ivy-cite
      org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
      org-ref-default-bibliography (list bibfile-path)
      org-ref-notes-function
      (lambda (thekey)
	(let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
	  (bibtex-completion-edit-notes
	   (list (car (org-ref-get-bibtex-key-and-file thekey))))))
      bibtex-completion-bibliography (list bibfile-path)
      bibtex-completion-pdf-field "file"  ; For Zotero, see .bib file
      bibtex-completion-notes-path "~/org/org-roam/" ; One org-file for per publications
      bibtex-completion-notes-template-multiple-files
      (concat
        "${title}\n"
        "* TODO Notes\n"
        ":PROPERTIES:\n"
        ":NOTER_DOCUMENT: ${file}\n"
        ":END:\n\n"
        ))
