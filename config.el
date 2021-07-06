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

;;;; Leader keys and keybindings

(setq doom-localleader-key ",")
(load! "keybindings.el")
;;;; Org Mode

(setq org-directory "~/Nextcloud/Notes/org/")

(load! "org-mode.el")
(load! "functions.el")

;;;; File Templates

(set-file-template! "\\.org" :trigger "__default.org" :mode 'org-mode)
(set-file-template! "\\.sh" :trigger "__default.sh" :mode 'sh-mode)
(set-file-template! "\\.html?$" :trigger "__default.html" :mode 'web-mode)
(set-file-template! "\\.el" :trigger "__default.el" :mode 'emacs-lisp-mode)
(set-file-template! "Blorg/snuffy-org/.+\\.org?$" :trigger "__snuffy-org.org" :mode 'org-mode)
(set-file-template! "Sites/snuffy.org/.+\\.org?$" :trigger "__snuffy-org-posts.org" :mode 'org-mode)
(set-file-template! "salt-master.+\\.org?$" :trigger "__salt-master.org" :mode 'org-mode)
(set-file-template! "NSI-Documentation/[^/]+\\.org?$" :trigger "__NSI-Documentation.org":mode 'org-mode)
(set-file-template! "NSI-Documentation/.+/[^/]+\\.org?$" :trigger "__NSI-Documentation.org" :mode 'org-mode)
(set-file-template! "NSI-Documentation/tipjar/[^/]+\\.org?$" :trigger "__NSI-Documentation-tipjar.org" :mode 'org-mode)
(set-file-template! "NSI-Documentation/TVA/[^/]+\\.org?$" :trigger "__NSI-Documentation-TVA.org" :mode 'org-mode)
(set-file-template! "NSI-Documentation/TVA/ScanReports/.+[^/]+\\.org?$" :trigger "__NSI-Documentation-TVA-scanreport.org" :mode 'org-mode)
(set-file-template! "NSI-Documentation/Patching/.+[^/]+\\.org?$" :trigger "__NSI-Documentation-Patching-Notes.org" :mode 'org-mode)
(set-file-template! "masons/[^/].+\\.org?$" :trigger "__masonsMeetingMinuets.org" :mode 'org-mode)
(set-file-template! "daily/[^/].+\\.org?$" :trigger "__defaultRoamDaily.org" :mode 'org-mode)
(set-file-template! "/[0-9]\\{8\\}.org$" :trigger "__defaultJournal.org" :mode 'org-mode)


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

