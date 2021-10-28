;;; org.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory (if (equal machine "workstation")
                         "~/Private/org/"
                       "~/org/"))

(after! org
  ;; Use a separate inbox to not mess up SyncThing and Orgzly
  (setq! +org-capture-todo-file (concat org-directory "desktop_inbox.org"))

  ;; Always display in-line iamges at start.
  (setq! org-startup-with-inline-images t)

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
  )
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
