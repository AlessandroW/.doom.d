;;; config/org-roam.el -*- lexical-binding: t; -*-

;; ORG ROAM
(after! org-roam
  (setq! org-roam-directory my/org-roam-directory
         org-roam-capture-templates
         '(("d" "default" plain
            "%?\n* Folgezettel\n\n* Related\n\n* Index\n\n* References"
            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n#+filetags: \n\n")
            :unnarrowed t))

         org-roam-capture-ref-templates
         '(("r" "ref" plain
            "%?\n* Folgezettel\n\n* Related\n\n* Index\n\n* References\n- [[${ref}][Source]]"
            :if-new (file+head "web/${slug}.org"
                               "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n#+filetags:\n\n")
            :roam_refs "${ref}"
            :unnarrowed t)
           ("t" "ref" plain
            "%?"
            :if-new (file+head "web/${ref}.org" "#+TITLE: ${title}\n")
            :unnarrowed t))))
