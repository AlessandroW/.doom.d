;;; config/reading.el -*- lexical-binding: t; -*-

;; Pretty reading stack for Org and Markdown.
;; Goal: document-like prose, while keeping source blocks, tables and metadata
;; editing-safe and fixed-pitch.

(defvar my/reading-default-width 90
  "Default centered body width for pretty reading buffers.")

(defvar my/reading-wide-width 120
  "Wide centered body width for code/table-heavy reading buffers.")

(defun my/reading--subtle-background ()
  "Return a theme-derived background for code/block cards."
  (or (face-background 'tooltip nil t)
      (face-background 'mode-line nil t)
      (face-background 'default nil t)))

(defun my/pretty-reading-buffer-p ()
  "Return non-nil when the current buffer should use pretty reading polish."
  (derived-mode-p 'org-mode 'markdown-mode 'gfm-mode 'markdown-view-mode))

(defun my/pretty-reading--refresh-layout (&optional buffer)
  "Refresh reading layout for BUFFER after window/mode toggles settle."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (when (and (my/pretty-reading-buffer-p)
                 (bound-and-true-p olivetti-mode)
                 (fboundp 'olivetti-set-width))
        (olivetti-set-width olivetti-body-width)))))

(defun my/pretty-reading-setup ()
  "Enable the shared pretty reading stack in the current buffer."
  (setq-local display-line-numbers nil
              line-spacing 0.12
              x-underline-at-descent-line t)
  (when (fboundp 'visual-line-mode)
    (visual-line-mode 1))
  (when (fboundp 'mixed-pitch-mode)
    (mixed-pitch-mode 1))
  (when (fboundp 'olivetti-mode)
    (olivetti-mode 1))
  (when (fboundp 'valign-mode)
    (valign-mode 1))
  ;; Some modes/window changes compute margins before the window has settled.
  ;; Refresh once on the next tick; this mirrors the "toggle writeroom fixes it"
  ;; effect without letting writeroom own the buffer font.
  (run-at-time 0 nil #'my/pretty-reading--refresh-layout (current-buffer)))

(defun my/pretty-reading-toggle-width ()
  "Toggle the current reading buffer between default and wide body widths."
  (interactive)
  (unless (bound-and-true-p olivetti-mode)
    (olivetti-mode 1))
  (setq-local olivetti-body-width
              (if (equal olivetti-body-width my/reading-default-width)
                  my/reading-wide-width
                my/reading-default-width))
  (when (fboundp 'olivetti-set-width)
    (olivetti-set-width olivetti-body-width))
  (message "Reading width: %s" olivetti-body-width))

(use-package! olivetti
  :defer t
  :config
  (setq! olivetti-min-body-width 60
         olivetti-body-width my/reading-default-width
         olivetti-style t
         olivetti-margin-width 12))

(use-package! valign
  :defer t)

(after! mixed-pitch
  ;; Doom's :ui zen supplies mixed-pitch. Keep common structured text fixed-pitch.
  (setq mixed-pitch-set-height t)
  (dolist (face '(org-block org-block-begin-line org-block-end-line
                  org-code org-verbatim org-table org-formula
                  org-meta-line org-document-info-keyword org-special-keyword
                  org-property-value org-drawer org-checkbox
                  markdown-code-face markdown-pre-face markdown-inline-code-face
                  markdown-table-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

(after! org
  (setq! org-hide-emphasis-markers t
         org-pretty-entities t
         org-startup-indented t
         org-startup-with-inline-images t
         ;; Headings should read like document headings, not outline bullets.
         org-superstar-headline-bullets-list '(" "))

  (let ((block-bg (my/reading--subtle-background)))
    (custom-set-faces!
      ;; Document typography.
      '(org-document-title :inherit variable-pitch :height 1.55 :weight bold)
      '(org-level-1 :inherit variable-pitch :height 1.35 :weight bold)
      '(org-level-2 :inherit variable-pitch :height 1.23 :weight semi-bold)
      '(org-level-3 :inherit variable-pitch :height 1.13 :weight semi-bold)
      '(org-level-4 :inherit variable-pitch :height 1.06 :weight semi-bold)
      '(org-level-5 :inherit variable-pitch :height 1.0 :weight normal)
      '(org-level-6 :inherit variable-pitch :height 1.0 :weight normal)
      '(org-level-7 :inherit variable-pitch :height 1.0 :weight normal)
      '(org-level-8 :inherit variable-pitch :height 1.0 :weight normal)
      `(org-link :inherit link :weight normal :underline ,(face-foreground 'link nil t))

      ;; Keep source/structure editable, but quiet.
      `(org-block :inherit fixed-pitch :background ,block-bg :extend t)
      `(org-block-begin-line :inherit (fixed-pitch shadow) :height 0.85 :background ,block-bg :extend t)
      `(org-block-end-line :inherit (fixed-pitch shadow) :height 0.85 :background ,block-bg :extend t)
      '(org-code :inherit (fixed-pitch org-code))
      '(org-verbatim :inherit (fixed-pitch org-verbatim))
      '(org-table :inherit fixed-pitch)
      '(org-formula :inherit fixed-pitch)
      '(org-meta-line :inherit (fixed-pitch shadow) :height 0.85)
      '(org-document-info-keyword :inherit (fixed-pitch shadow) :height 0.85)
      '(org-special-keyword :inherit (fixed-pitch shadow) :height 0.85)
      '(org-property-value :inherit (fixed-pitch shadow) :height 0.85)
      '(org-drawer :inherit (fixed-pitch shadow) :height 0.85)
      '(org-tag :inherit shadow :height 0.85)
      '(org-date :inherit shadow)
      '(org-indent :inherit org-hide)))

  (use-package! org-modern
    :hook (org-mode . org-modern-mode)
    :config
    (setq! org-modern-star nil
           org-modern-hide-stars nil
           org-modern-list '((?+ . "•") (?- . "•") (?* . "•"))
           org-modern-checkbox '((?X . "☑") (?- . "◩") (?\s . "☐"))
           org-modern-block-fringe nil
           org-modern-table nil
           org-modern-tag t
           org-modern-todo t))

  (add-hook 'org-mode-hook #'my/pretty-reading-setup))

(after! markdown-mode
  (setq! markdown-hide-markup t
         markdown-fontify-code-blocks-natively t)

  (let ((block-bg (my/reading--subtle-background)))
    (custom-set-faces!
      '(markdown-header-face-1 :inherit variable-pitch :height 1.45 :weight bold)
      '(markdown-header-face-2 :inherit variable-pitch :height 1.30 :weight bold)
      '(markdown-header-face-3 :inherit variable-pitch :height 1.18 :weight semi-bold)
      '(markdown-header-face-4 :inherit variable-pitch :height 1.10 :weight semi-bold)
      '(markdown-header-face-5 :inherit variable-pitch :height 1.03 :weight normal)
      '(markdown-header-face-6 :inherit variable-pitch :height 1.0 :weight normal)
      `(markdown-code-face :inherit fixed-pitch :background ,block-bg)
      `(markdown-pre-face :inherit fixed-pitch :background ,block-bg :extend t)
      '(markdown-inline-code-face :inherit fixed-pitch)
      '(markdown-table-face :inherit fixed-pitch)
      '(markdown-markup-face :inherit shadow :height 0.9)
      '(markdown-url-face :inherit shadow)
      '(markdown-link-face :inherit link :weight normal)))

  (add-hook 'markdown-mode-hook #'my/pretty-reading-setup)
  (add-hook 'gfm-mode-hook #'my/pretty-reading-setup)
  (add-hook 'markdown-view-mode-hook #'my/pretty-reading-setup))

(after! writeroom-mode
  ;; Doom's writeroom toggles `mixed-pitch-mode' as a local effect. Since our
  ;; reading buffers are always pretty, turning writeroom off should not leave
  ;; them in fixed-pitch. Re-apply the stack after writeroom has restored state.
  (add-hook 'writeroom-mode-disable-hook
            (defun my/pretty-reading-after-writeroom-disable-h ()
              (when (my/pretty-reading-buffer-p)
                (run-at-time 0 nil #'my/pretty-reading-setup)))))

(map! :leader
      :desc "Toggle reading width"
      "t W" #'my/pretty-reading-toggle-width)
