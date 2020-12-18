;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alessandro Wollek"
      user-mail-address "a@wollek.dev"

      ;; So opt for manual completion:
      ;; https://www.monolune.com/configuring-company-mode-in-emacs/
      company-idle-delay 0
      company-minimum-prefix-length 2
      company-selection-wrap-around t

      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil)


;; CUSTOM KEYBINDINGS
(map! "C-x C-b" 'ivy-switch-buffer ) ;; Don't open the buffer menu when pressing Ctrl for too long.

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;

(setq-default left-margin-width 1 right-margin-width 1)
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(when (> (display-pixel-height) 1200)
  (setq doom-font (font-spec :family "monospace" :size 20)
        doom-variable-pitch-font (font-spec :family "sans" :size 20)
        doom-big-font (font-spec :family "monospace" :size 24)))

(when (< (display-pixel-height) 1200)
  (setq doom-font (font-spec :family "monospace" :size 14)
        doom-variable-pitch-font (font-spec :family "sans" :size 14)
        doom-big-font (font-spec :family "monospace" :size 20)))
;; (setq +pretty-code-enabled-modes nil)
;; (setq doom-font (font-spec :family "monospace" :size 19 );;:weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 19))
;; (setq doom-unicode-font (font-spec :name "" :size 19))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; x-clipboard fix
(setq x-selection-timeout 10)

;; ORG MODE
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; The built-in calendar mode mappings for org-journal
;; conflict with evil bindings
(map!
 (:map calendar-mode-map
   :n "o" #'org-journal-display-entry
   :n "p" #'org-journal-previous-entry
   :n "n" #'org-journal-next-entry
   :n "O" #'org-journal-new-date-entry))

;; Local leader (<SPC m>) bindings for org-journal in calendar-mode
;; I was running out of bindings, and these are used less frequently
;; so it is convenient to have them under the local leader prefix
(map!
 :map (calendar-mode-map)
 :localleader
 "w" #'org-journal-search-calendar-week
 "m" #'org-journal-search-calendar-month
 "y" #'org-journal-search-calendar-year)




(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://ergoemacs.org/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))

(setq-hook! 'writeroom-mode-hook
  line-spacing 0.5
  fill-column 100
  )


(setq org-agenda-files
        (append (file-expand-wildcards "~/org/schedule/*.org")
                (file-expand-wildcards "~/org/projects/*.org")
                (file-expand-wildcards "~/org/*.org")))

(after! org
  ;; BEtter Bullets
  ;; From http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
  (font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; From https://lepisma.xyz/2017/10/28/ricing-org-mode/
  (setq header-line-format " ")
  (setq org-tags-column 0)
  (setq org-superstar-headline-bullets-list '("⠀󠀠"));;("▕󠀠󠀠"󠀠"●" "○"))
  (setq org-ellipsis "…")
  (setq org-hide-emphasis-markers t)
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  ))

;; Org-Super-Agend
;; From https://www.rousette.org.uk/archives/doom-emacs-tweaks-org-journal-and-org-super-agenda/
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-custom-commands
        '(("c" "Super view"
           ((alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:log t)
                            (:name "Today's tasks"
                                   :file-path "schedule/day.org")
                            (:name "Inbox"
                                   :file-path "./inbox.org")
                            (:name "This Week's tasks"
                                   :file-path "schedule/week.org")
                            (:name "Work"
                                   :file-path "projects/snapaddy.org")
                            (:name "Next Week's tasks"
                                   :file-path "schedule/next_week.org")
                            (:name "This Months's tasks"
                                   :file-path "schedule/this_month.org")
                            (:name "Due Today"
                                   :deadline today
                                   :order 2)
                            (:name "Projects"
                                   :file-path "projects/")
                            (:name "Scheduled Soon"
                                   :scheduled future
                                   :order 8)
                            (:name "Overdue"
                                   :deadline past
                                   :order 7)
                            (:name "Meetings"
                                   :and (:todo "MEET" :scheduled future)
                                   :order 10)))))))))
  :config
  (org-super-agenda-mode))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Jupyter Notebook Evil Keys
;; Open docs with SPC h f map\!
;; EIN Keymap: https://github.com/millejoh/emacs-ipython-notebook#keymap-c-h-m
;; Spacemacs Keymap: https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/ipython-notebook/packages.el
(setq ein:output-area-inlined-images t)
(map! :map ein:notebook-mode-map
      :ni "<S-return>" 'ein:worksheet-execute-cell-and-goto-next-km
      :ni "<C-return>" 'ein:worksheet-execute-cell
      :n "g j" 'ein:worksheet-goto-next-input-km
      :n "g k" 'ein:worksheet-goto-prev-input-km
      )

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-excluded-commands '(self-insert-command))


;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(map! (:when (featurep! :ui window-select)
      "C-ö" #'other-window
      "C-;" #'other-window
      ))


(setq-default flycheck-flake8-maximum-line-length 99)

;;; :default
;;; smart-parens
;;; See: https://ebzzry.io/en/emacs-pairs/


(defmacro def-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")))

(map! :map smartparens-mode-map
      :ni "C-M-f" 'sp-forward-sexp
      :ni "C-M-b" 'sp-backward-sexp
      :v "(" 'sp-wrap-round
      :v "{" 'sp-wrap-curly
      :v "[" 'sp-wrap-square
      :v ")" 'sp-wrap-round
      :v "}" 'sp-wrap-curly
      :v "]" 'sp-wrap-square
      :ni "C-c ("  'wrap-with-parens
      :ni "C-c ["  'wrap-with-brackets
      :ni "C-c {"  'wrap-with-braces
      :ni "C-c '"  'wrap-with-single-quotes
      :ni "C-c \"" 'wrap-with-double-quotes
      :ni "C-c `"  'wrap-with-back-quotes
      :ni "M-[" 'sp-backward-unwrap-sexp
      :ni "M-]" 'sp-unwrap-sexp)


;;; :ui
(map! (:when (featurep! :ui popup)
       "C-ä"   #'+popup/toggle))

;;; :org
;;;
(defvar +org-capture-todo-file "inbox.org"
  "Default target for todo entries.
Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "PROJ(p)" "STRT(s)" "|" "DONE(d)" "KILL(k)")))
  (setq org-todo-keyword-faces
        '(("TODO" . warning)
          ("NEXT" . error)
          ("STRT" . success)
          ("WAIT" . font-lock-keyword-face)
          ("PROJ" . org-headline-todo)
          ))
  )

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
          (message "Regex did not match"))))))

  (defun alessandrow-org-roam-mode-before-save-hook()
    "Before-save-hook for org-roam to add hugo_tags if roam_tags exist."
    (when (string-equal major-mode "org-mode")
      (alessandrow-duplicate-roam-tags-as-hugo-tags)))
  (add-hook 'before-save-hook 'alessandrow-org-roam-mode-before-save-hook)
)
(setq org-roam-capture-templates
'(("d" "default" plain
        (function org-roam-capture--get-point)
        "%?\n* References"
        :file-name "%<%Y%m%d%H%M%S>-${slug}"
        :head "#+TITLE: \"${title}\"\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+setupfile:~/org/org-roam/hugo_setup.org\n#+roam_alias:\n#+roam_tags:\n\n"
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

;; ORG ROAM
(add-hook 'after-init-hook 'org-roam-mode)
(setq org-roam-directory "~/org/org-roam")

;; ORG with Zotero
;; based on https://rgoswami.me/posts/org-note-workflow/#zotero

;; FIX [[org:org-roam/an_orgmode_note_workflow_rohit_goswami_reflections.org][Creating A org-Roam Zotero Workflow]]
;; (setq org_notes org-roam-directory
;;       zot_bib "~/Dokumente/References/my_zotero_library.bib"
;;       deft-directory org-directory)

;; Helm-Bibtex
;; Open PDF in Evince not Firefox
(setq helm-external-programs-associations '(("pdf" . (if (eq system-type 'gnu/linux) "open" "evince"))))
;; Helm-Bibtex config
(setq reftex-default-bibliography '("~/Dokumente/References/Public/my_zotero_library.bib")
      org-ref-completion-library 'org-ref-ivy-cite
      org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
      org-ref-default-bibliography '("~/Dokumente/References/Public/my_zotero_library.bib")
      org-ref-notes-function
      (lambda (thekey)
	(let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
	  (bibtex-completion-edit-notes
	   (list (car (org-ref-get-bibtex-key-and-file thekey))))))
      bibtex-completion-bibliography '("~/Dokumente/References/Public/my_zotero_library.bib")
      bibtex-completion-pdf-field "file"  ; For Zotero, see .bib file
      bibtex-completion-notes-path "~/org/org-roam/" ; One org-file for per publications
      bibtex-completion-notes-template-multiple-files
      (concat
        "${title}\n"
        "* TODO Notes\n"
        ":PROPERTIES:\n"
        ":NOTER_DOCUMENT: ${file}\n"
        ":END:\n\n"
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

;; EMACS APPLICATION FRAMEWORK
;; https://github.com/manateelazycat/emacs-application-framework#install
;; (use-package! eaf :load-path "~/projects/repositories/emacs-application-framework")
