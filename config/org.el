;;; org.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory (if (equal machine "workstation")
                         "~/Private/org/"
                       "~/org/"))

(after! org
  ;; Keybindings
  ;;
  (map! :mode 'org-mode :i "C-c TAB" #'org-table-toggle-column-width)
  (map! :mode 'org-mode :i "C-c ]" #'org-ref-insert-cite-link)



  ;; Use a separate inbox to not mess up SyncThing and Orgzly
  (setq! +org-capture-todo-file (concat org-directory "desktop_inbox.org"))

  ;; Tame org-open-file, which uses org-file-apps, and finally mailcap.el
  (setq! org-file-apps
      '((auto-mode . emacs)
        ("\\.x?html?\\'" . "xdg-open %s")
        ("\\.pdf\\'" . "xdg-ope, \"%s\"")
        ("\\.pdf::\\([0-9]+\\)\\'" . "xdg-open \"%s\" -p %1")
        ("\\.pdf.xoj" . "xournal %s")))

  (add-hook 'org-mode-hook 'hl-todo-mode)

  ;; Always display in-line iamges at start.
  (setq! org-startup-with-inline-images t)
  (setq! org-startup-with-latex-preview t)


  (add-to-list 'org-emphasis-alist
               '("/" (:foreground "#ff6655" :slant italic)))
  (add-to-list 'org-emphasis-alist
               '("*" (:foreground "#282c34" :background "#ECBE7B" :weight bold)))

  (setq! org-agenda-files (-concat (mapcar (lambda (filename) (concat org-directory filename)) '("next_action.org" "inbox.org" "desktop_inbox.org"))
                                   (directory-files-recursively (concat org-directory "projects") org-agenda-file-regexp)))
  (custom-set-faces!
    '(org-link :weight bold :underline "#51afef" :foreground "brightwhite")
    '(org-level-1 :height 1.4 :foreground "grey" :weight bold :family "SF Pro")
    '(org-level-2 :height 1.2 :foreground "grey" :weight bold :family "SF Pro")
    '(org-level-3 :height 1.1 :foreground "grey" :weight bold :family "SF Pro")
    '(org-level-4 :height 1.1 :foreground "grey" :weight bold :family "SF Pro")
    '(org-level-5 :height 1.0 :foreground "grey" :weight bold :family "SF Pro")
    '(org-level-6 :height 1.0 :foreground "grey" :weight bold :family "SF Pro")
    '(org-document-title :height 1.5 :foreground "grey" :family "SF Pro")
    '(org-indent :inherit org-hide)
    )
  (setq org-superstar-headline-bullets-list '(" ")
        org-superstar-item-bullet-alist '((?* . ?⋆)
                                          (?+ . ?‣)
                                          (?- . ?•)))
  ;; Helm-Bibtex
  (setq! zotero-dir (if (equal machine "workstation") "~/Documents/"
                     "~/Dokumente/References/"))

  ;; Helm-Bibtex config
  (setq! reftex-default-bibliography (concat zotero-dir "my_zotero_library.bib")
        org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-default-bibliography (concat zotero-dir "my_zotero_library.bib")      org-ref-notes-function
        (lambda (thekey)
          (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
            (bibtex-completion-edit-notes
             (list (car (org-ref-get-bibtex-key-and-file thekey))))))
        bibtex-completion-bibliography (concat zotero-dir "my_zotero_library.bib")
        bibtex-completion-pdf-field "file"      ; For Zotero, see .bib file
        bibtex-completion-notes-path org-roam-directory ; One org-file for per publications
        bibtex-completion-notes-template-multiple-files
        (concat
         "${title}\n"
         "* TODO Notes\n"
         ":PROPERTIES:\n"
         ":NOTER_DOCUMENT: ${file}\n"
         ":END:\n\n"
         ))


  (defun my/copy-idlink-to-clipboard()
    "Copy an ID link with the
headline to killring, if no ID is there then create a new unique
ID.  This function works only in org-mode or org-agenda buffers.

The purpose of this function is to easily construct id:-links to
org-mode items. If its assigned to a key it saves you marking the
text and copying to the killring.

© Rainer König, https://koenig-haunstetten.de/2018/02/17/improving-my-orgmode-workflow/
"
    (interactive)
    (when (eq major-mode 'org-agenda-mode) ;switch to orgmode
      (org-agenda-show)
      (org-agenda-goto))
    (when (eq major-mode 'org-mode) ; do this only in org-mode buffers
      (let ((mytmphead (nth 4 (org-heading-components)))
            (mytmpid (funcall 'org-id-get-create)) (mytmplink))
        (progn
          (setq mytmplink (format "[[id:%s][%s]]" mytmpid mytmphead))
          (kill-new mytmplink)
          (message "Copied %s to killring (clipboard)" mytmplink)))))

  (map! :leader
        :mode 'org-mode
        :desc "Copy ID-link to clipboard"
        "m I" #'my/copy-idlink-to-clipboard))
(defun no-line-numbers ()
  (setq-local display-line-numbers nil)
  )
(add-hook 'org-mode-hook 'no-line-numbers)

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))
(use-package! org-appear
  :after org
  :config (setq org-hide-emphasis-markers t)
  :hook (org-mode . org-appear-mode))
(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-face 'variable-pitch))
