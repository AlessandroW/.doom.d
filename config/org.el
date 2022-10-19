;;; org.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory (if (equal machine "workstation")
                         "~/Private/org/"
                       "~/org/"))
(after! evil
;; Use emacs keybindings in the org-mode calendar
;; https://emacs.stackexchange.com/questions/14115/disable-evil-mode-in-calendar
(evil-set-initial-state 'calendar-mode 'emacs))

(after! org
  ;; Keybindings
  ;;
  (map! :mode 'org-mode :i "C-c TAB" #'org-table-toggle-column-width)
  (map! :mode 'org-mode :i "C-c ]" #'org-ref-insert-cite-link)

  ;; Insert Time Samp
  (defun org-insert-timestamp ()
    "Insert current time."
    (interactive)
    (org-insert-time-stamp (current-time)))


  ;; Org Mode Clock Persistent
  (setq! org-clock-persist 'history)
  (org-clock-persistence-insinuate)


  ;; HACK ignore Invalid base64 data errors. These will stop further execution.
  ;; I get the error message:
  ;; Error in post-command-hook (org-roam-buffer--redisplay-h): (error "Invalid base64 data")
  ;; See: https://github.com/hlissner/doom-emacs/issues/3185
  (defadvice! no-errors/+org-inline-image-data-fn (_protocol link _description)
    :override #'+org-inline-image-data-fn
    "Interpret LINK as base64-encoded image data. Ignore all errors."
    (ignore-errors
      (base64-decode-string link)))

  ;; Use a separate inbox to not mess up SyncThing and Orgzly
  (setq! +org-capture-todo-file (concat org-directory "desktop_inbox.org"))

  ;; Tame org-open-file, which uses org-file-apps, and finally mailcap.el
  (setq! org-file-apps
         '((auto-mode . emacs)
           ("\\.x?html?\\'" . "xdg-open %s")
           ("\\.pdf\\'" . "xdg-open \"%s\"")
           ("\\.pdf::\\([0-9]+\\)\\'" . "xdg-open \"%s\" -p %1")
           ("\\.pdf.xoj" . "xournal %s")))

  (add-hook 'org-mode-hook 'hl-todo-mode)

  ;; Always display in-line iamges at start.
  (setq! org-startup-with-inline-images t)
  (setq! org-startup-with-latex-preview t)


  (add-to-list 'org-emphasis-alist
               '("/" (:slant italic)))
  (add-to-list 'org-emphasis-alist
               '("*" (:foreground "#ff6655" :weight bold)))

  (setq! org-agenda-files (-concat (mapcar (lambda (filename) (concat org-directory filename)) '("private.org" "inbox.org" "desktop_inbox.org" "work.org"))
                                   (directory-files-recursively (concat org-directory "projects") org-agenda-file-regexp)))
  ;; ;; This should be done according to a theme
  (custom-set-faces!
    `(org-link :weight medium :underline ,(face-foreground 'link) :foreground ,(face-foreground 'default))
    '(org-level-1 :height 1.4)
    '(org-level-2 :height 1.2)
    '(org-level-3 :height 1.1)
    '(org-level-4 :height 1.1)
    '(org-level-5 :height 1.0)
    '(org-level-6 :height 1.0)
    `(org-document-title :height 1.5 :foreground ,(face-foreground 'default))
    '(org-indent :inherit org-hide)
    )

  (defun my/org-line-spacing()
    ;; (kill-local-variable 'line-spacing)

    (setq-local default-text-properties
                '(line-height 1.5
                              line-spacing 0.1)
                x-underline-at-descent-line t))
  (add-hook 'mixed-pitch-mode-hook 'my/org-line-spacing)

  (setq org-superstar-headline-bullets-list '(" ")
        org-superstar-item-bullet-alist '((?* . ?⋆)
                                          (?+ . ?‣)
                                          (?- . ?•)))
  ;; Ivy-Bibtex
  (setq! zotero-dir (if (equal machine "workstation") "~/Documents/"
                      "~/Dokumente/References/"))
  ;; Ivy-Bibtex config
  (setq! bibtex-completion-bibliography (concat zotero-dir "my_zotero_library.bib")
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
    (when (eq major-mode 'org-mode)     ; do this only in org-mode buffers
      (let ((mytmphead (nth 4 (org-heading-components)))
            (mytmpid (funcall 'org-id-get-create)) (mytmplink))
        (progn
          (setq mytmplink (format "[[id:%s][%s]]" mytmpid mytmphead))
          (kill-new mytmplink)
          (message "Copied %s to killring (clipboard)" mytmplink)))))

  (map! :leader
        :mode 'org-mode
        :desc "Copy ID-link to clipboard"
        "m I" #'my/copy-idlink-to-clipboard)
  (defun my/org-set-creation-date-heading-property ()
    (save-excursion
      (org-back-to-heading)
      (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %T]"))))

  (add-hook 'org-insert-heading-hook 'my/org-set-creation-date-heading-property)
  (defun no-line-numbers ()
    (setq-local display-line-numbers nil))
  (add-hook 'org-mode-hook 'no-line-numbers)

  (add-hook 'find-file-hook #'my/set-org-agenda-files-faces)
  (defun my/set-org-agenda-files-faces ()
    "Set the faces for the ORG-AGENDA-FILES.
TODO lists need a different faces than org documents."
    (when (-contains-p org-agenda-files buffer-file-name)
      (face-remap-add-relative 'org-level-1 :height 1.0 :weight 'light)
      (face-remap-add-relative 'org-link :height 1.0 :weight 'light)))

(defun ereslibre/is-entry-of-type (type entry)
  (let ((entry-link (plist-get (car entry) :entry)))
    (string-match (format "^%s/" type) entry-link)))

(defun ereslibre/sitemap (title list)
  ;; From https://github.com/ereslibre/ereslibre.es/blob/main/config/default.el
  (let ((entries (mapconcat (lambda (entry) (car entry))  (cdr list) "\n")))
    (format "#+TITLE: %s
#+SUBTITLE: Data Science, Software Development, and Emacs.
#+SETUPFILE: ~/.doom.d/org-templates/blog-level-0.org
#+OPTIONS: toc:nil\n
#+begin_export html
%s
#+end_export
"
            title entries )))

  ;; BLOG
  ;; Inspired by https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html
  (require 'ox-publish)
  (defun my/org-sitemap-date-entry-format (entry style project)
    "Format ENTRY in org-publish PROJECT Sitemap format ENTRY ENTRY STYLE format that includes date.
     © Thomas Ingram, CC-BY-SA 4.0"
    (let ((filename (org-publish-find-title entry project)))
      (if (= (length filename) 0)
          (format "*%s*" entry)
        ;; BUG it's still .org not .html!
        ;; Fixed here? https://github.com/ereslibre/ereslibre.es/blob/a717ce71821aa69928034e8517025a26dd582051/config/default.el#L232
        (format "<p><pre>%s</pre><a href='%s'>%s</a> </p>"
                (format-time-string "%Y-%m-%d"
                                    (org-publish-find-date entry project))
                entry
                filename))))
  (setq org-publish-project-alist
        '(("blog-notes"
           :base-directory "~/org/blog/org"
           :base-extension "org"
           :publishing-directory "~/org/blog/public/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4           ; Just the default for this project.
           :auto-preamble t
           :auto-sitemap t       ; Generate sitemap automagically... see https://orgmode.org/manual/Site-map.html
           :sitemap-filename "index.org" ; ... call it index.org (it's the default)...
           :sitemap-function ereslibre/sitemap
           :sitemap-title "Alessandro Wollek's Blog"
           :sitemap-sort-files anti-chronologically
           :sitemap-format-entry my/org-sitemap-date-entry-format
           )

          ("blog-static"
           :base-directory "~/org/blog/org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/org/blog/public/"
           :recursive t
           :publishing-function org-publish-attachment)

          ("blog" :components ("blog-notes" "blog-static"))))
  ;; END OF after! ORG
  )

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))
(use-package! org-appear
  :after org
  :config (setq org-hide-emphasis-markers t)
  :hook (org-mode . org-appear-mode))

(defun my/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))
