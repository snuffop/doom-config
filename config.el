;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;; Top
(setq user-full-name "Marty Buchaus"
      user-mail-address "marty@dabuke.com")

;;;; Fonts

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 10.5 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Sans" )
      doom-unicode-font (font-spec :family "Ubuntu" :size 12)
      doom-big-font (font-spec :family "DejaVu Sans Mono" :size 19))

;;;; Theme
(setq doom-theme 'doom-dracula)

;;;;  Line Numbers
(setq display-line-numbers-type 'relative)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Nextcloud/Notes/org/")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(load! org-mode.el)
