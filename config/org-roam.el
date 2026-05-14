;;; config/org-roam.el -*- lexical-binding: t; -*-

;; ORG ROAM
(after! org-roam
  (setq! org-roam-directory my/org-roam-directory
         org-roam-capture-templates
         '(("d" "default" plain
            "%?\n* Folgezettel\n\n* Related\n\n* Index\n\n* References"
            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n# Local Variables:\n# org-attach-id-dir: \"../data\"\n# End:\n#+filetags: \n\n")
            :unnarrowed t))

         org-roam-capture-ref-templates
         '(("r" "ref" plain
            "%?\n* Folgezettel\n\n* Related\n\n* Index\n\n* References\n- [[${ref}][Source]]"
            :if-new (file+head "web/${slug}.org"
                               "#+TITLE: ${title}\n#+CREATED: %u\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n# Local Variables:\n# org-attach-id-dir: \"../../data\"\n# End:\n#+filetags:\n\n")
            :roam_refs "${ref}"
            :unnarrowed t)
           ("t" "ref (title as filename)" plain
            "%?"
            :if-new (file+head "web/${ref}.org" "#+TITLE: ${title}\n")
            :unnarrowed t))
         org-roam-dailies-capture-templates
         '(("d" "default" entry
            "* Planned\n%?\n* Today\n* Tomorrow\n* Folgezettel\n\n* Related\n\n* Index\n- [[id:338ecc5c-5083-4efa-ae80-7d9d682161c8][Work Journal]]\n* References"
            :if-new (file+head "%<%Y-%m-%d>.org"
                               "#+TITLE: %<%Y-%m-%d>\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n# Local Variables:\n# org-attach-id-dir: \"../../data\"\n# End:\n#+filetags: :daily:\n\n")
            :unnarrowed t))
         )

;;;###autoload
  (defun my/org-roam-dailies-latest ()
    "Open the latest daily note"
    (interactive)
    (find-file (car (last (org-roam-dailies--list-files)))))
;;;###autoload
  (defun my/org-roam-dailies-insert-today ()
    "Insert a link to today's daily note and create it if it doesn't exist yet."
    (interactive)
    ;; Get today's date in the format used by org-roam-dailies
    (let* ((today (format-time-string "%Y-%m-%d"))
           (title (format-time-string "%A, %Y-%m-%d")) ; Format: "Monday, 2023-05-15"
           (node (org-roam-node-from-title-or-alias today)))

      ;; If node doesn't exist in the database, create it first
      (unless node
        ;; Capture today's daily note without prompt
        (save-window-excursion
          (org-roam-dailies-capture-today)
          ;; Use keyboard macro to finalize the capture
          (org-capture-finalize))
        ;; Refresh the node reference after creation
        (setq node (org-roam-node-from-title-or-alias today)))

      ;; Now insert a link to the node
      (if node
          (insert (org-link-make-string
                   (concat "id:" (org-roam-node-id node))
                   (org-roam-node-title node)))
        (message "Failed to find or create today's daily note"))))

    (map! :leader
        :mode org-mode
        "n r I" #'my/org-roam-dailies-insert-today
        "n r l" #'my/org-roam-dailies-latest))
