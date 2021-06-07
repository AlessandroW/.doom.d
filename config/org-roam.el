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
