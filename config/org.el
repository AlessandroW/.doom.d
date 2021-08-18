;;; org.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory "~/org/")

(after! org
  ;; Use a separate inbox to not mess up SyncThing and Orgzly
  (setq! +org-capture-todo-file "~/org/desktop_inbox.org")

  (add-to-list 'org-emphasis-alist
             '("/" (:foreground "#ff6655" :weight italic)))
  (add-to-list 'org-emphasis-alist
             '("*" (:foreground "#282c34" :background "#ECBE7B" :weight bold)))

  (setq! org-agenda-files (-concat
                           '("~/org/next_action.org" "~/org/inbox.org" "~/org/desktop_inbox.org")
                           (directory-files-recursively "~/org/projects" org-agenda-file-regexp))))

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))
(use-package! org-appear
  :after org
  :config (setq org-hide-emphasis-markers t)
  :hook (org-mode . org-appear-mode))
(use-package! olivetti
  :after org
  ;; :hook (org-roam-mode . olivetti-mode)
  :config
    (setq olivetti-min-body-width 50
          olivetti-body-width 100
          olivetti-style t ; fantastic new layout
          olivetti-margin-width 12))

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-face 'variable-pitch))
