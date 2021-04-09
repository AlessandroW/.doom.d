;;; config/org-roam.el -*- lexical-binding: t; -*-

;; ORG ROAM
(add-hook 'after-init-hook 'org-roam-mode)
(setq org-roam-directory "~/org/org-roam")

(setq org-roam-capture-templates
'(("d" "default" plain
        (function org-roam-capture--get-point)
        "%?\n* References"
        :file-name "%<%Y%m%d%H%M%S>-${slug}"
        :head "#+TITLE: \"\"${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n#+roam_alias:\n#+roam_tags:\n\n"
        :unnarrowed t)
        ))

(setq org-roam-capture-ref-templates
'(("r" "ref" plain (function org-roam-capture--get-point)
        "%?\n* References\n- [[${ref}][Source]]"
        :file-name "web/${slug}"
        :head "#+TITLE: \"\"\n#+ROAM_ALIAS: \"${title}\"\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n#+roam_key: ${ref}\n#+roam_tags:\n\n"
        :unnarrowed t)
        ("t" "ref" plain (function org-roam-capture--get-point)
        "%?"
        :file-name "${ref}"
        :unnarrowed t)
        ))
