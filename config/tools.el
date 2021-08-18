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

;; Debugging
;; © Aaron Harris, https://emacs.stackexchange.com/a/19582
(defun my/call-logging-hooks (command &optional verbose)
  "Call COMMAND, reporting every hook run in the process.
Interactively, prompt for a command to execute.

Return a list of the hooks run, in the order they were run.
Interactively, or with optional argument VERBOSE, also print a
message listing the hooks."
  (interactive "CCommand to log hooks: \np")
  (let* ((log     nil)
         (logger (lambda (&rest hooks)
                   (setq log (append log hooks nil)))))
    (my/with-advice
        ((#'run-hooks :before logger))
      (call-interactively command))
    (when verbose
      (message
       (if log "Hooks run during execution of %s:"
         "No hooks run during execution of %s.")
       command)
      (dolist (hook log)
        (message "> %s" hook)))
    log))
(defmacro my/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))
