;;; org.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory "~/org/")

;; (after! org
;;   ;; Use a separate inbox to not mess up SyncThing and Orgzly
;;   (setq! +org-capture-todo-file "~/org/desktop_inbox.org")

  ;; (setq! org-agenda-files (-concat
  ;;                          '("~/org/next_action.org" "~/org/inbox.org" "~/org/desktop_inbox.org")
  ;;                          (directory-files-recursively "~/org/projects" org-agenda-file-regexp)))
                           ;; )
