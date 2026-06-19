;;; config/reading.el -*- lexical-binding: t; -*-

;; Pretty reading stack for Org and Markdown.
;; Goal: document-like prose, while keeping source blocks, tables and metadata
;; editing-safe and fixed-pitch.

(defvar my/reading-default-width 52
  "Maximum default centered body width for pretty reading buffers.

This is intentionally lower than an 80-90 character prose target because
Olivetti measures in fixed-pitch frame columns, while our prose uses a narrower
proportional font. A width around 52 columns yields roughly 75-85 prose
characters per visual line with New York.")

(defvar my/reading-default-window-fraction 0.56
  "Default fraction of the window used for pretty reading buffers.")

(defvar my/reading-wide-width 90
  "Maximum wide centered body width for code/table-heavy reading buffers.")

(defvar my/reading-wide-window-fraction 0.84
  "Wide fraction of the window used for code/table-heavy reading buffers.")

(defvar-local my/reading-wide-p nil
  "Whether this buffer should use the wide pretty reading width.")

(defun my/reading--target-width ()
  "Return an adaptive Olivetti width for the current window."
  (let* ((max-width (if my/reading-wide-p my/reading-wide-width my/reading-default-width))
         (fraction (if my/reading-wide-p my/reading-wide-window-fraction my/reading-default-window-fraction))
         (window-width (max 1 (window-total-width)))
         (fractional-width (floor (* window-width fraction))))
    (max (if (boundp 'olivetti-minimum-body-width) olivetti-minimum-body-width 50)
         (min max-width fractional-width))))

(defun my/reading--first-available-font (&rest fonts)
  "Return the first available font family from FONTS."
  (catch 'font
    (dolist (font fonts)
      (when (member font (font-family-list))
        (throw 'font font)))
    (car fonts)))

(defvar my/reading-heading-font
  (my/reading--first-available-font "Avenir Next" "Avenir" "Helvetica Neue" "Helvetica" "sans")
  "Sans-serif font for document headings.")

(defface my/reading-metadata-face
  '((t :inherit shadow :height 0.82))
  "Face for subdued Org/Markdown metadata lines.")

(defface my/reading-block-guide-face
  '((t :inherit shadow))
  "Face for close inline Org block guides.")

(defface my/reading-callout-guide-face
  '((t :inherit my/reading-block-guide-face))
  "Face for close inline Org callout block guides.")

(defun my/reading--dark-color-p (color)
  "Return non-nil when COLOR is visually dark."
  (require 'color)
  (let ((rgb (color-name-to-rgb color)))
    (and rgb (< (apply #'+ rgb) 1.5))))

(defun my/reading--adjust-color (color light-percent dark-percent)
  "Lighten or darken COLOR by LIGHT-PERCENT or DARK-PERCENT."
  (require 'color)
  (if (my/reading--dark-color-p color)
      (color-lighten-name color light-percent)
    (color-darken-name color dark-percent)))

(defun my/reading--subtle-background ()
  "Return a theme-derived background for code/block cards."
  (let ((bg (or (face-background 'default nil t) "#ffffff")))
    (my/reading--adjust-color bg 7 4)))

(defun my/reading--blend-color (foreground background alpha)
  "Blend FOREGROUND over BACKGROUND by ALPHA and return a hex color."
  (require 'color)
  (require 'cl-lib)
  (let ((fg (color-name-to-rgb foreground))
        (bg (color-name-to-rgb background)))
    (if (and fg bg)
        (apply #'color-rgb-to-hex
               (cl-mapcar (lambda (f b) (+ (* alpha f) (* (- 1 alpha) b))) fg bg))
      foreground)))

(defun my/pretty-reading-buffer-p ()
  "Return non-nil when the current buffer should use pretty reading polish."
  (derived-mode-p 'org-mode 'markdown-mode 'gfm-mode
                  'markdown-view-mode 'gfm-view-mode))

(defun my/pretty-reading--refresh-layout (&optional buffer)
  "Refresh reading layout for BUFFER after window/mode toggles settle."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (when (and (my/pretty-reading-buffer-p)
                 (bound-and-true-p olivetti-mode)
                 (fboundp 'olivetti-set-width))
        (setq-local olivetti-body-width (my/reading--target-width))
        (olivetti-set-width olivetti-body-width)))))

(defun my/pretty-reading-refresh-frame-h (&optional frame)
  "Refresh adaptive reading widths in all visible windows on FRAME."
  (dolist (window (window-list frame 'no-minibuf))
    (with-current-buffer (window-buffer window)
      (when (and (my/pretty-reading-buffer-p)
                 (bound-and-true-p olivetti-mode))
        (with-selected-window window
          (my/pretty-reading--refresh-layout))))))

(add-hook 'window-size-change-functions #'my/pretty-reading-refresh-frame-h)
(add-hook 'window-configuration-change-hook #'my/pretty-reading-refresh-frame-h)

(defun my/pretty-reading-setup ()
  "Enable the shared pretty reading stack in the current buffer."
  (setq-local display-line-numbers nil
              line-spacing 0.12
              x-underline-at-descent-line t
              olivetti-body-width (my/reading--target-width))
  (when (fboundp 'visual-line-mode)
    (visual-line-mode 1))
  (when (fboundp 'mixed-pitch-mode)
    (mixed-pitch-mode 1))
  (when (fboundp 'olivetti-mode)
    (olivetti-mode 1))
  ;; `org-modern-table' and `valign' both rewrite table separators; together
  ;; they create doubled table-line artifacts. Keep `valign' for Markdown, and
  ;; let `org-modern' own Org tables.
  (when (and (fboundp 'valign-mode)
             (not (derived-mode-p 'org-mode)))
    (valign-mode 1))
  ;; Some modes/window changes compute margins before the window has settled.
  ;; Refresh once on the next tick; this mirrors the "toggle writeroom fixes it"
  ;; effect without letting writeroom own the buffer font.
  (run-at-time 0 nil #'my/pretty-reading--refresh-layout (current-buffer))
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'my/org-reading-block-guides-schedule))
    (my/org-reading-block-guides-schedule)))

(defun my/pretty-reading-toggle-width ()
  "Toggle the current reading buffer between default and wide body widths."
  (interactive)
  (unless (bound-and-true-p olivetti-mode)
    (olivetti-mode 1))
  (setq-local my/reading-wide-p (not my/reading-wide-p)
              olivetti-body-width (my/reading--target-width))
  (when (fboundp 'olivetti-set-width)
    (olivetti-set-width olivetti-body-width))
  (message "Reading width: %s" olivetti-body-width))

(use-package! olivetti
  :defer t
  :config
  (setq! olivetti-minimum-body-width 40
         olivetti-min-body-width 40
         olivetti-body-width my/reading-default-width
         olivetti-style t
         olivetti-margin-width 12))

(use-package! valign
  :defer t)

;; Open Markdown as a read-only reading view by default. Use `e' in the view
;; buffer to switch back to editable GFM mode.
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-view-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-view-mode))

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
         org-hidden-keywords '(title)
         org-pretty-entities t
         org-hide-leading-stars nil
         org-startup-indented nil
         org-fontify-quote-and-verse-blocks t
         org-fontify-whole-block-delimiter-line nil
         org-startup-with-inline-images t
         org-auto-align-tags nil
         org-tags-column 0
         org-catch-invisible-edits 'show-and-error
         org-special-ctrl-a/e t
         org-insert-heading-respect-content t
         org-ellipsis "…"
         ;; Headings should read like document headings, not outline bullets.
         org-superstar-headline-bullets-list '(" "))

  (defun my/pretty-reading-apply-faces (&rest _)
    "Apply theme-aware faces for pretty Org/Markdown reading."
    (let* ((default-bg (or (face-background 'default nil t) "#ffffff"))
           (block-bg (my/reading--subtle-background))
           (default-fg (or (face-foreground 'default nil t) "#000000"))
           ;; Metadata must stay quiet but readable on both Doom One and light
           ;; themes. Deriving it only from the background made it disappear on
           ;; saturated dark themes.
           (metadata-fg (my/reading--blend-color
                         default-fg default-bg
                         (if (my/reading--dark-color-p default-bg) 0.48 0.55)))
           (guide-fg (my/reading--blend-color
                      default-fg default-bg
                      (if (my/reading--dark-color-p default-bg) 0.30 0.24)))
           (callout-fg (or (face-foreground 'link nil t) guide-fg)))
      (custom-set-faces!
        ;; Document typography.
        `(org-document-title :inherit variable-pitch :family ,my/reading-heading-font :height 1.90 :weight bold :foreground ,default-fg)
        `(org-level-1 :inherit variable-pitch :family ,my/reading-heading-font :height 1.52 :weight bold :foreground ,default-fg)
        `(org-level-2 :inherit variable-pitch :family ,my/reading-heading-font :height 1.36 :weight bold :foreground ,default-fg)
        `(org-level-3 :inherit variable-pitch :family ,my/reading-heading-font :height 1.23 :weight bold :foreground ,default-fg)
        `(org-level-4 :inherit variable-pitch :family ,my/reading-heading-font :height 1.13 :weight semi-bold :foreground ,default-fg)
        `(org-level-5 :inherit variable-pitch :family ,my/reading-heading-font :height 1.06 :weight semi-bold :foreground ,default-fg)
        `(org-level-6 :inherit variable-pitch :family ,my/reading-heading-font :height 1.0 :weight semi-bold :foreground ,default-fg)
        `(org-level-7 :inherit variable-pitch :family ,my/reading-heading-font :height 1.0 :weight normal :foreground ,default-fg)
        `(org-level-8 :inherit variable-pitch :family ,my/reading-heading-font :height 1.0 :weight normal :foreground ,default-fg)
        `(org-link :inherit link :weight normal :underline ,(face-foreground 'link nil t))

        ;; Keep source/structure editable, but quiet. Recompute on theme changes.
        `(my/reading-metadata-face :inherit fixed-pitch :foreground ,metadata-fg :height 0.78)
        `(my/reading-block-guide-face :foreground ,guide-fg :weight normal)
        `(my/reading-callout-guide-face :foreground ,callout-fg :weight semi-bold)
        ;; Let org-modern own block names; custom overlays draw close guides.
        `(org-block :inherit fixed-pitch :background unspecified :extend nil)
        `(org-quote :inherit variable-pitch :slant italic :background unspecified :extend nil)
        `(org-verse :inherit variable-pitch :background unspecified :extend nil)
        `(org-block-begin-line :inherit my/reading-metadata-face :background unspecified :extend nil)
        `(org-block-end-line :inherit my/reading-metadata-face :background unspecified :extend nil)
        '(org-code :inherit (fixed-pitch org-code))
        '(org-verbatim :inherit (fixed-pitch org-verbatim))
        '(org-table :inherit fixed-pitch)
        '(org-formula :inherit fixed-pitch)
        '(org-checkbox :height 1.35 :weight normal)
        '(org-meta-line :inherit my/reading-metadata-face)
        '(org-document-info-keyword :inherit my/reading-metadata-face)
        '(org-special-keyword :inherit my/reading-metadata-face)
        '(org-property-value :inherit my/reading-metadata-face)
        '(org-drawer :inherit my/reading-metadata-face)
        '(org-tag :inherit my/reading-metadata-face)
        '(org-date :inherit my/reading-metadata-face)
        '(org-indent :inherit org-hide)

        `(markdown-header-face-1 :inherit variable-pitch :family ,my/reading-heading-font :height 1.55 :weight bold :foreground ,default-fg)
        `(markdown-header-face-2 :inherit variable-pitch :family ,my/reading-heading-font :height 1.38 :weight bold :foreground ,default-fg)
        `(markdown-header-face-3 :inherit variable-pitch :family ,my/reading-heading-font :height 1.24 :weight bold :foreground ,default-fg)
        `(markdown-header-face-4 :inherit variable-pitch :family ,my/reading-heading-font :height 1.14 :weight semi-bold :foreground ,default-fg)
        `(markdown-header-face-5 :inherit variable-pitch :family ,my/reading-heading-font :height 1.06 :weight semi-bold :foreground ,default-fg)
        `(markdown-header-face-6 :inherit variable-pitch :family ,my/reading-heading-font :height 1.0 :weight normal :foreground ,default-fg)
        `(markdown-code-face :inherit fixed-pitch :background ,block-bg)
        `(markdown-pre-face :inherit fixed-pitch :background ,block-bg :extend t)
        '(markdown-inline-code-face :inherit fixed-pitch)
        '(markdown-table-face :inherit fixed-pitch)
        '(markdown-markup-face :inherit shadow :height 0.9)
        '(markdown-url-face :inherit shadow)
        '(markdown-blockquote-face :inherit shadow :slant normal)
        '(markdown-link-face :inherit link :weight normal))))

  (my/pretty-reading-apply-faces)
  (advice-add #'load-theme :after #'my/pretty-reading-apply-faces)

  (defun my/org-pretty-reading-font-lock ()
    "Add extra font-lock polish for pretty Org reading."
    (font-lock-add-keywords
     nil
     '(("^\\(?:#\\+\\)?\\(?:CREATED\\|LAST_MODIFIED\\|FILETAGS\\|filetags\\|SETUPFILE\\|setupfile\\):.*$"
        0 'my/reading-metadata-face prepend)
       ("^[ \\t]*:\\(?:PROPERTIES\\|END\\|[[:alnum:]_@#%]+\\):.*$"
        0 'my/reading-metadata-face prepend)
       ("^\\([ \\t]*[-+*][ \\t]+\\)\\(\\[[ X-]\\]\\)"
        (1 '(face org-hide display "") prepend)))
     'append))

  (add-hook 'org-mode-hook #'my/org-pretty-reading-font-lock)

  (defvar-local my/org-reading-block-guide-overlays nil
    "Close inline block guide overlays in the current Org buffer.")

  (defvar-local my/org-reading-block-guide-timer nil
    "Debounce timer for refreshing Org block guide overlays.")

  (defun my/org-reading-block-guides-clear ()
    "Remove close inline Org block guide overlays from the current buffer."
    (mapc #'delete-overlay my/org-reading-block-guide-overlays)
    (setq my/org-reading-block-guide-overlays nil))

  (defun my/org-reading--callout-block-p (element)
    "Return non-nil when ELEMENT is an Org special block used as a callout."
    (and (eq (org-element-type element) 'special-block)
         (member (downcase (or (org-element-property :type element) ""))
                 '("note" "info" "tip" "hint" "important" "warning" "caution"
                   "danger" "error" "question" "quote" "example" "todo"))))

  (defun my/org-reading-block-guides-refresh ()
    "Draw close inline guides for Org blocks without using the window fringe."
    (when (derived-mode-p 'org-mode)
      (setq my/org-reading-block-guide-timer nil)
      (my/org-reading-block-guides-clear)
      (save-excursion
        (save-restriction
          (widen)
          (let ((ast (org-element-parse-buffer)))
            (org-element-map ast '(src-block quote-block example-block verse-block special-block)
              (lambda (element)
                (let ((beg (org-element-property :begin element))
                      (end (copy-marker (org-element-property :end element)))
                      (face (if (my/org-reading--callout-block-p element)
                                'my/reading-callout-guide-face
                              'my/reading-block-guide-face)))
                  (goto-char beg)
                  (let ((first-line (line-beginning-position)))
                    (while (< (point) end)
                      (let* ((line-beg (line-beginning-position))
                             (next-line (save-excursion (forward-line 1) (point)))
                             (guide (cond ((and (= line-beg first-line) (>= next-line end)) "├ ")
                                          ((= line-beg first-line) "╭ ")
                                          ((>= next-line end) "╰ ")
                                          (t "│ ")))
                             (ov (make-overlay line-beg line-beg)))
                        (overlay-put ov 'category 'my/org-reading-block-guide)
                        (overlay-put ov 'priority 80)
                        (overlay-put ov 'evaporate t)
                        (overlay-put ov 'before-string (propertize guide 'face face))
                        (push ov my/org-reading-block-guide-overlays))
                      (forward-line 1)))))))))))

  (defun my/org-reading-block-guides-schedule (&rest _)
    "Debounce close inline Org block guide refreshes."
    (when (derived-mode-p 'org-mode)
      (when (timerp my/org-reading-block-guide-timer)
        (cancel-timer my/org-reading-block-guide-timer))
      (setq my/org-reading-block-guide-timer
            (run-with-idle-timer 0.18 nil
                                 (lambda (buffer)
                                   (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (unless my/pretty-reading-edit-mode
                                         (my/org-reading-block-guides-refresh)))))
                                 (current-buffer)))))

  (defun my/org-reading-block-guides-setup ()
    "Enable close inline block guides in the current Org buffer."
    (add-hook 'after-change-functions #'my/org-reading-block-guides-schedule nil t)
    (my/org-reading-block-guides-schedule))

  (add-hook 'org-mode-hook #'my/org-reading-block-guides-setup)

  (defvar-local my/pretty-reading-edit-mode nil
    "Non-nil when the current buffer is temporarily in source-editing view.")

  (defun my/pretty-reading-toggle-view ()
    "Toggle between pretty reading and source editing view for this buffer."
    (interactive)
    (setq-local my/pretty-reading-edit-mode (not my/pretty-reading-edit-mode))
    (if my/pretty-reading-edit-mode
        (progn
          (when (bound-and-true-p olivetti-mode) (olivetti-mode -1))
          (when (bound-and-true-p mixed-pitch-mode) (mixed-pitch-mode -1))
          (when (bound-and-true-p valign-mode) (valign-mode -1))
          (when (and (derived-mode-p 'org-mode) (bound-and-true-p org-modern-mode))
            (org-modern-mode -1))
          (when (derived-mode-p 'org-mode)
            (setq-local org-hide-emphasis-markers nil)
            (my/org-reading-block-guides-clear))
          (font-lock-flush)
          (message "Pretty reading: edit/source view"))
      (when (derived-mode-p 'org-mode)
        (setq-local org-hide-emphasis-markers t)
        (when (fboundp 'org-modern-mode) (org-modern-mode 1)))
      (my/pretty-reading-setup)
      (font-lock-flush)
      (message "Pretty reading: document view")))

  (use-package! org-modern
    :hook (org-mode . org-modern-mode)
    :config
    (setq! org-modern-star nil
           org-modern-hide-stars t
           org-modern-list '((?+ . "•") (?- . "•") (?* . "•"))
           org-modern-checkbox `((?X . ,(propertize "☑" 'face '(:height 1.35)))
                                 (?- . ,(propertize "◩" 'face '(:height 1.35)))
                                 (?\s . ,(propertize "☐" 'face '(:height 1.35))))
           org-modern-timestamp nil
           org-modern-block-name t
           ;; Do not use org-modern's fringe bracket: Olivetti centers the text
           ;; column but the fringe stays at the window edge. We draw close
           ;; inline guides with overlays instead.
           org-modern-block-fringe nil
           org-modern-table t
           org-modern-table-vertical 2
           org-modern-table-horizontal 0.15
           org-modern-tag t
           org-modern-todo t))

  (add-hook 'org-mode-hook #'my/pretty-reading-setup))

(after! markdown-mode
  (defun my/markdown-view-edit ()
    "Switch the current Markdown view buffer to editable GFM mode."
    (interactive)
    (gfm-mode)
    (my/pretty-reading-setup)
    (message "Markdown edit mode"))

  (map! :map markdown-view-mode-map
        :desc "Edit Markdown" "e" #'my/markdown-view-edit)
  (when (boundp 'gfm-view-mode-map)
    (map! :map gfm-view-mode-map
          :desc "Edit Markdown" "e" #'my/markdown-view-edit))

  (setq! markdown-hide-markup t
         markdown-hide-urls t
         markdown-fontify-code-blocks-natively t
         markdown-blockquote-display-char "▌"
         markdown-make-gfm-checkboxes-buttons t
         markdown-gfm-uppercase-checkbox t)

  (add-hook 'markdown-mode-hook #'my/pretty-reading-setup)
  (add-hook 'gfm-mode-hook #'my/pretty-reading-setup)
  (add-hook 'markdown-view-mode-hook #'my/pretty-reading-setup)
  (add-hook 'gfm-view-mode-hook #'my/pretty-reading-setup))

(add-hook 'after-change-major-mode-hook
          (defun my/pretty-reading-after-major-mode-h ()
            "Apply pretty reading to buffers that entered a reading mode early."
            (when (my/pretty-reading-buffer-p)
              (my/pretty-reading-setup))))

(use-package! markdown-xwidget
  :after markdown-mode
  :commands markdown-xwidget-preview-mode
  :config
  (setq markdown-xwidget-command
        "pandoc -f gfm+tex_math_dollars+tex_math_single_backslash -t html --mathjax"
        markdown-xwidget-github-theme "light"
        markdown-xwidget-code-block-theme "github"
        markdown-xwidget-mermaid-theme "default")
  (map! :map markdown-mode-command-map
        :desc "Markdown xwidget preview"
        "x" #'markdown-xwidget-preview-mode)
  (map! :leader
        :desc "Markdown xwidget preview"
        "m x" #'markdown-xwidget-preview-mode))

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
      "t W" #'my/pretty-reading-toggle-width
      :desc "Toggle reading/editing view"
      "t R" #'my/pretty-reading-toggle-view)
