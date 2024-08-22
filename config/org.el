;;; org.el -*- lexical-binding: t; -*-

(after! evil
  ;; Use emacs keybindings in the org-mode calendar
  ;; https://emacs.stackexchange.com/questions/14115/disable-evil-mode-in-calendar
  (evil-set-initial-state 'calendar-mode 'emacs))

(after! org
  (setq! org-clock-persist 'history
         org-startup-with-inline-images t
         org-startup-with-latex-preview nil
         ;; Hide those org heading bullets
         org-superstar-headline-bullets-list '(" ")
         ;; Prettier lists
         org-superstar-item-bullet-alist '((?* . ?⋆)
                                           (?+ . ?‣)
                                           (?- . ?•))

         ;; Org-Ref Ivy-Bibtex config
         zotero-dir my/zotero-dir
         bibtex-completion-bibliography zotero-dir
         bibtex-completion-pdf-field "file" ; For Zotero, see .bib file
         bibtex-completion-notes-path org-roam-directory ; One org-file for per publications
         bibtex-completion-notes-template-multiple-files
         (concat
          "${title}\n"
          "* TODO Notes\n"
          ":PROPERTIES:\n"
          ":NOTER_DOCUMENT: ${file}\n"
          ":END:\n\n"
          ))
  ;; Org Mode Clock Persistent
  (org-clock-persistence-insinuate)

  ;; Different font sizes for each heading.
  (custom-set-faces!
    `(org-link :weight medium :underline ,(face-foreground 'link) :foreground ,(face-foreground 'default))
    '(org-level-1 :height 1.4)
    '(org-level-2 :height 1.2)
    '(org-level-3 :height 1.1)
    '(org-level-4 :height 1.1)
    '(org-level-5 :height 1.0)
    '(org-level-6 :height 1.0)
    `(org-document-title :height 1.5 :foreground ,(face-foreground 'default))
    '(org-indent :inherit org-hide))

  (defun my/org-copy-hierarchical-idlink-to-clipboard ()
    "Copy an ID link with the hierarchical headline structure to the killring.

If no ID is present for the current heading, a new unique ID is created.
This function works only in `org-mode' or `org-agenda-mode' buffers.

The purpose of this function is to easily construct id:-links to
org-mode items with context. If assigned to a key, it saves you
from marking the text and copying to the killring manually.

Example:
With the following Org file:

#+title: File

* Heading 1
** Heading 1.1
*** Heading 1.1.1
*** Heading 1.1.2

If the point is inside 'Heading 1.1.2', the function will copy
the following link to the killring:

[[id:SOME-ID][File.org > Heading 1 > Heading 1.1 > Heading 1.1.2]]

© Original version: Rainer König, https://koenig-haunstetten.de/2018/02/17/improving-my-orgmode-workflow/
© Hierarchical version: Alessandro Wollek, https://claude.ai/chat/4c09f781-4afc-4a40-bbbf-b2432010ccd1"
    (interactive)
    (if (eq major-mode 'org-agenda-mode)
        (progn
          (org-agenda-show)
          (org-agenda-goto)))
    (if (eq major-mode 'org-mode)
        (progn
          (let* ((my-id (funcall 'org-id-get-create))
                 (my-headings (org-get-outline-path t t))
                 (my-file (buffer-file-name (buffer-base-buffer))))
            (setq my-link (format "[[id:%s][%s%s]]" my-id
                                  (if my-file (concat (file-name-nondirectory my-file) " > "))
                                  (mapconcat #'identity my-headings " > ")))
            (kill-new my-link)
            (message "Copied %s to killring (clipboard)" my-link)))))

  (defun my/org-set-creation-date-heading-property ()
    "Insert CREATED property to a heading."
    (save-excursion
      (org-back-to-heading)
      (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %T]"))))

  (defun my/org-update-last-modified ()
    "Update the LAST_MODIFIED property in the current Org file.

If the LAST_MODIFIED property exists, it will be updated with the
current timestamp. If it doesn't exist, the LAST_MODIFIED property
will be added after the TITLE property, only if the TITLE property
exists in the file. If the TITLE property doesn't exist, no action
will be taken."
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^#\\+LAST_MODIFIED:" nil t)
            (let ((current-time (format-time-string "[%Y-%m-%d %a %H:%M]")))
              (move-beginning-of-line nil)
              (delete-line)
              (insert (concat "#+LAST_MODIFIED: " current-time "\n")))
          (goto-char (point-min))
          (if (re-search-forward "^#\\+TITLE:" nil t)
              (progn
                (end-of-line)
                (insert "\n#+LAST_MODIFIED: [" (format-time-string "%Y-%m-%d %a %H:%M") "]"))
            ;; Do nothing if TITLE property doesn't exist
            nil)))))

  (defun my/no-line-numbers ()
    "Disable line-numbers locally."
    (setq-local display-line-numbers nil))

  (defun my/org-line-spacing()
    (setq-local default-text-properties
                '(line-height 1.5
                  line-spacing 0.1)
                x-underline-at-descent-line t))

  ;; HOOCKS
  (add-hook 'org-mode-hook #'my/org-line-spacing)
  (add-hook 'org-insert-heading-hook #'my/org-set-creation-date-heading-property)
  (add-hook 'org-mode-hook #'my/no-line-numbers)
  (add-hook 'before-save-hook #'my/org-update-last-modified)

  ;; KEYBINDINGS
  (map! :leader
        :mode org-mode
        "m I" #'my/org-copy-hierarchical-idlink-to-clipboard
        "C-;" #'other-window))

;; Hide emphasis markers
(use-package! org-appear
  :after org
  :config (setq org-hide-emphasis-markers t)
  :hook (org-mode . org-appear-mode))

;; Prettier Org document margins
(use-package! olivetti
  :after org
  :config
  (setq! olivetti-min-body-width 50
         olivetti-body-width 120
         olivetti-style t ; fantastic new layout
         olivetti-margin-width 12)
  :hook (org-mode . olivetti-mode))
