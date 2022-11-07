;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "local.el") ;; Machine specific settings.
(setq user-full-name "Alessandro Wollek"
      user-mail-address "a@wollek.dev")


(load! "config/org.el")
(load! "config/org-roam.el")
(load! "config/ui.el")
(load! "config/checkers.el")
(load! "config/tools.el")
(load! "config/completion.el")

(map! "C-x C-b" 'ivy-switch-buffer ) ;; Don't open the buffer menu when pressing Ctrl for too long.
(map! "C-ö" #'other-window
      "C-;" #'other-window)
;; Use the mouse to go to the previous and next buffer.
(map! "<mouse-8>" 'previous-buffer )
(map! "<mouse-9>" 'next-buffer )

(defun copy-rectangle-to-system-clipboard (start end)
  "Like `copy-rectangle-as-kill', but also copy to system clipboard."
  (interactive "r")
  (call-interactively #'copy-rectangle-as-kill)
  (with-temp-buffer
    (yank-rectangle)
    (delete-trailing-whitespace)
    (funcall interprogram-cut-function (buffer-string))))
(map! "C-x r M-c" #'copy-rectangle-to-system-clipboard)

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(after! lsp-mode
  ;; Ignore these directories
  ;; See https://github.com/emacs-lsp/lsp-mode/issues/1085
  (dolist (dir '(
                 "[/\\\\]env"
                 "[/\\\\]venv"
                 "[/\\\\]build"
                 "[/\\\\]node_modules"
                 ))
    (push dir lsp-file-watch-ignored))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pylsp-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd-15")
                    :major-modes '(c++-mode)
                    :remote? t
                    :server-id 'clangd-remote))

  )

;; Projetile
(after! projectile
  (setq! projectile-indexing-method 'alien ;; No projectile post-processing, better for remote work
         projectile-enable-caching nil))


(after! python
  (add-hook! 'python-mode-hook #'origami-mode)
  (map! :n "z c" #'origami-toggle-node)
  (map! :n "z o" #'origami-toggle-node)
  (map! :n "z m" #'origami-toggle-all-nodes)
  )


(after! latex
  ;; Use pdf-tools to open PDF files
  ;; https://emacs.stackexchange.com/a/19475
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; https://apple.stackexchange.com/a/278069
  (when IS-MAC
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
    (setq exec-path (append exec-path '("/Library/TeX/texbin/")))))


(defun open-kitty ()
  "Open a new kitty instance."
  (interactive)
  (call-process "kitty" nil 0 nil))
(map! :leader :n "k"  #'open-kitty)
