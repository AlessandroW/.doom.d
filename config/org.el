;;; org.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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
  (add-hook 'before-save-hook 'alessandrow-org-roam-mode-before-save-hook)


  (add-to-list 'org-link-frame-setup '(file . find-file-other-window))

  ;; https://superuser.com/questions/530363/emacs-org-mode-how-to-disable-visual-line-wrap-for-agenda-buffers-only
  (add-hook 'org-mode-hook (lambda () (visual-line-mode -1)))
)
