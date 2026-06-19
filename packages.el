;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Highlight word under point
(package! idle-highlight-mode)
;; Zotero integration in org-mode
(package! org-ref)
;; Personal AI assistant, https://github.com/SFTtech/emacs-elaiza
(package! elaiza :recipe (:local-repo "/Users/aw/src/work/elaiza" :build (:not compile)))

;; Elaiza dependencies
(package! llm
  :recipe (:host github
           :repo "ahyatt/llm")
  :pin "874d762") ;; 0.23.0

(package! plz
  :recipe (:host github
           :repo "alphapapa/plz.el")
  :pin "c579f03")

(package! plz-event-source
  :recipe (:host github
           :repo "r0man/plz-event-source")
  :pin "e9c54b3")

(package! plz-media-type
  :recipe (:host github
           :repo "r0man/plz-media-type")
  :pin "4404f21")


;; Hide emphasis markers in org
(package! org-appear :recipe (:host github :repo "awth13/org-appear"))
;; Pretty reading stack for org/markdown
(package! doom-nano-themes
  :recipe (:host github :repo "ronisbr/doom-nano-themes"))
(package! org-modern)
(package! valign)
;; Prettier org/markdown margins
(package! olivetti)
;; Show others what I type
(package! keycast)
;; Grammar check
;; (package! languagetool)
(package! casual-suite)
;; https://github.com/jdtsmith/eglot-booster
(package! eglot-booster :recipe (:host github :repo "jdtsmith/eglot-booster"))
