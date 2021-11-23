;;; config/org-roam.el -*- lexical-binding: t; -*-

;; ORG ROAM
(setq org-roam-directory (if (equal machine "workstation")
                             "~/Private/org/org-roam"
                           "~/org/org-roam"))
(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?\n* Folgezettel\n\n* Related\n\n* References"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n#+filetags: :zettelkastenv2:\n\n")
         :unnarrowed t)))

(setq org-roam-capture-ref-templates
      '(("r" "ref" plain
         "%?\n* Folgezettel\n\n* Related\n\n* References\n- [[${ref}][Source]]"
         :if-new (file+head "web/${slug}.org"
                            "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n#+filetags: :zettelkastenv2:literature_note:\n\n")
         :roam_refs "${ref}"
         :unnarrowed t)
        ("t" "ref" plain
         "%?"
         :if-new (file+head "web/${ref}.org" "#+TITLE: ${title}\n")
         :unnarrowed t)
        ))

;; ORG ROAM UI
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; OX-HUGO
;; FROM
;; https://github.com/jethrokuan/dots/blob/86b554e99253d7adb96f2e6d558d08cad2c91ee3/.doom.d/config.el#L573
(after! org
  (defun alessandrow-duplicate-current-line ()
    "Duplicates the current line below."
    (let ((current-line (thing-at-point 'line)))
      (insert "\n")
      (insert current-line)))

  (defun alessandrow-duplicate-roam-tags-as-hugo-tags ()
    "Duplicate the roam_tags as hugo_tags to export them with ox-hugo."
    (let ((current-point (point)))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^#\\+hugo_tags.+$" nil t)
            (goto-char point)
          (let ((roam_tag (re-search-forward "^#\\+roam_tags.+$" nil t)))
            (if roam_tag
                (progn (goto-char roam_tag)
                       (alessandrow-duplicate-current-line )
                       (re-search-backward "^#\\+roam_tags:" nil t)
                       (replace-match "#+hugo_tags:")
                       (goto-char current-point)
                       (forward-line 2))
              (message "Regex did not match")))))))

  (defun alessandrow-org-roam-mode-before-save-hook()
    "Before-save-hook for org-roam to add hugo_tags if roam_tags exist."
    (when (string-equal major-mode "org-mode")
      (alessandrow-duplicate-roam-tags-as-hugo-tags)))
  ;; (add-hook 'before-save-hook 'alessandrow-org-roam-mode-before-save-hook)

  )

;;(use-package! nroam
  ;; :after org-roam
  ;; :config
  ;; (setq +org-roam-open-buffer-on-find-file nil)
  ;; (add-hook 'org-mode-hook 'nroam-setup-maybe)
  ;; )

(use-package! olivetti
  :after org
  :config
  (setq olivetti-min-body-width 50
        olivetti-body-width 100
        olivetti-style t ; fantastic new layout
        olivetti-margin-width 12)
  (defun me/olivetti-mode ()
    "Start olivetti."
    (olivetti-mode))
  (add-hook 'org-mode-hook 'me/olivetti-mode))

(use-package! org-transclusion
  :defer
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))
