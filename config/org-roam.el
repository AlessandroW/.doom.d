;;; config/org-roam.el -*- lexical-binding: t; -*-

;; ORG ROAM
(add-hook 'after-init-hook 'org-roam-mode)
(setq org-roam-directory "~/org/org-roam")

(setq org-roam-capture-templates
'(("d" "default" plain
        (function org-roam-capture--get-point)
        "%?\n* Folgezettel\n\n* Related\n\n* References"
        :file-name "%<%Y%m%d%H%M%S>-${slug}"
        :head "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n#+roam_alias:\n#+roam_tags: \"zettelkastenv2\"\n\n"
        :unnarrowed t)
        ))

(setq org-roam-capture-ref-templates
'(("r" "ref" plain (function org-roam-capture--get-point)
        "%?\n* Folgezettel\n\n* Related\n\n* References\n- [[${ref}][Source]]"
        :file-name "web/${slug}"
        :head "#+TITLE: \n#+ROAM_ALIAS: \"${title}\"\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n#+roam_key: ${ref}\n#+roam_tags: \"zettelkastenv2\" \"literature note\"\n\n"
        :unnarrowed t)
        ("t" "ref" plain (function org-roam-capture--get-point)
        "%?"
        :file-name "${ref}"
        :unnarrowed t)
        ))

;; Org-Roam-Sever
(setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 9090
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)


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


  (add-to-list 'org-link-frame-setup '(file . find-file-other-window))

  ;; https://superuser.com/questions/530363/emacs-org-mode-how-to-disable-visual-line-wrap-for-agenda-buffers-only
  (add-hook 'org-mode-hook (lambda () (visual-line-mode -1)))
)
