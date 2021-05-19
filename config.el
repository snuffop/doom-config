;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Marty Buchaus"
      user-mail-address "marty@dabuke.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 10.5 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "sans" :size 10))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Nextcloud/Notes/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


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

;;;; Spacemacs Config

(add-hook 'doom-init-ui-hook #'spacemacs/home)
(remove-hook 'org-load-hook #'+org-init-keybinds-h)

;;;;; Layouts
(spacemacs|define-custom-layout "@Org"
  :binding "o"
  :body
  (find-file "~/Nextcloud/Notes/org/0mobile.org"))

(spacemacs|define-custom-layout "@SnuffyOrg"
  :binding "b"
  :body
  (find-file "~/Sites/snuffy.org/content-org/DailyNotes/Daily Notes 2021.org"))

(spacemacs|define-custom-layout "@chezmoi"
  :binding "d"
  :body
  (find-file "~/.local/share/chezmoi/README.org"))

(spacemacs|define-custom-layout "@Ledger"
  :binding "l"
  :body
  (find-file "~/Nextcloud/Documents/File Cabinet/Personal/ledger/ledger.ledger"))

(spacemacs|define-custom-layout "@Rackspace"
  :binding "r"
  :body
  (find-file "~/Source/NSI/NSI-Documentation/README.org"))

(spacemacs|define-custom-layout "@Salt"
  :binding "s"
  :body
  (find-file "~/Source/NSI/salt-master/README.org"))


;;;; Wakatime

(use-package! wakatime-mode
  :hook (doom-first-buffer . global-wakatime-mode)
  :config
  (setq wakatime-cli-path "/usr/bin/wakatime")
  (setq wakatime-api-key (auth-source-pass-get 'secret "Application/wakatime/apikey")))
