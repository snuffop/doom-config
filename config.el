;;; $doomdir/config.el --- My Emacs Config File -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2021, marty buchaus, all rights reserved.
;; created:  1 November 2021
;;
;;;; Notes
;;
;;  2021 11 18 Update clean Install and config
;;  2021 10 12  added code from Stuff from  https://github.com/Artawower/.doom/blob/main/config.el#L308
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code
;;;; GLOBAL

(setq user-full-name "Marty Buchaus")
(setq user-mail-address "marty@dabuke.com")
(setq epg-gpg-program "/usr/bin/gpg2")

(setq-default enable-local-variables t)            ; allow for reading the local variables file
(setq-default window-combination-resize t)
(setq-default left-margin-width 1)                 ; Define new widths
(setq-default right-margin-width 2)                ; Define new widths.
(setq-default x-stretch-cursor t)

(setq scroll-margin 2)                             ; it's nice to maintain a little margin
(setq evil-want-fine-undo t)                       ; by default while in insert all changes are one big blob. be more granular
(setq undo-limit 80000000)                         ; raise undo-limit to 80mb
(setq auto-save-default t)                         ; nobody likes to loose work, i certainly don't
(setq confirm-kill-emacs nil)                      ; stop hounding me and quit
(setq display-time-24hr-format t)                  ; I wonder what this does
(setq password-cache-expiry nil)                   ; i can trust my computers ... can't i?
(setq read-process-output-max (* 1024 1024))
(setq truncate-string-ellipsis "…")                ; unicode ellispis are nicer than "...", and also save /precious/ space
(setq warning-minimum-level :emergency)

(setq garbage-collection-messages nil)

(after! gcmh
  (setq gcmh-high-cons-threshold 67108864))  ; 33554432 32mb, or 67108864 64mb, or *maybe* 128mb, BUT NOT 512mb

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)  ; Make the scratch buffer start in lisp mode

(display-time-mode 1)                              ; enable time in the mode-line

(global-subword-mode 1)                            ; CamelCase and it makes refactoring slightly Essie

(after! projectile
  (setq projectile-indexing-method 'alien)
  (setq projectile-project-search-path '("~/Source")))
  ;; (setq projectile-project-root-files-bottom-up (remove ".git" projectile-project-root-files-bottom-up)))


(set-window-buffer nil (current-buffer))
(setenv "zstd" "/usr/bin/zstd")

;;;;; VTERM

(setq vterm-kill-buffer-on-exit t)
(setq vterm-always-compile-module t)               ; Always compile the vterm module
(setq vterm-shell "/usr/bin/zsh")

;;;; UI
;;;;; SET FONTS

(cond (IS-MAC
       (setq doom-font (font-spec :family "monospace" :size 15 :weight 'regular )))
      (t

       (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 15 :weight 'regular )
             doom-variable-pitch-font (font-spec :family "Ubuntu" :style "Regular" :size 15 :weight 'regular)
             doom-unicode-font (font-spec :family "symbola" :size 15)
             doom-big-font (font-spec :family "DejaVu Sans Mono" :size 24))))

;;;;; FACES

      (custom-set-faces!
        '(font-lock-comment-face :slant italic)
        '(font-lock-keyword-face :slant italic))
(setq global-prettify-symbols-mode t)

(custom-set-faces!
 '(mode-line :family "firacode nerd font mono" :height 100)
 '(mode-line-inactive :family "firacode nerd font mono" :height 100))

;;;;; THEME

(setq doom-theme 'doom-dracula )

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t))

;;;;; MODELINE

(after! doom-modeline
  (setq all-the-icons-scale-factor 1.1)
  (setq doom-modeline-enable-word-count t)
  (setq doom-themes-padded-modeline t)
  (setq auto-revert-check-vc-info t)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-major-mode-color-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon (display-graphic-p))
  (setq doom-modeline-vcs-max-length 60)

  (add-hook! 'doom-modeline-mode-hook
    (progn
      (set-face-attribute 'header-line nil
                          :background (face-background 'mode-line)
                          :foreground (face-foreground 'mode-line))
      )))

;;;;; DASHBOARD

(custom-theme-set-faces! 'doom-dracula
  '(doom-dashboard-banner :foreground "red" :background "#000000" :weight bold)
  '(doom-dashboard-footer :inherit font-lock-constant-face)
  '(doom-dashboard-footer-icon :inherit all-the-icons-red)
  '(doom-dashboard-loaded :inherit font-lock-warning-face)
  '(doom-dashboard-menu-desc :inherit font-lock-string-face)
  '(doom-dashboard-menu-title :inherit font-lock-function-name-face))

(setq fancy-splash-image (expand-file-name "banners/doom-emacs-slant-out-color.png" doom-private-dir))

;;;;; LINE NUMBERS

(setq display-line-numbers-type 'relative)

;; remove numbers from these modes

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;;; PACKAGES
;;;;; AUTOINSERT
(defun marty/autoinsert-yas-expand ()
  (let ((template ( buffer-string )))
    (delete-region (point-min) (point-max))
    (yas-expand-snippet template)
    (evil-insert-state)))

(use-package! autoinsert
  :init
  (setq auto-insert-query nil)
  (setq auto-insert-directory (expand-file-name "templates" doom-private-dir))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  :config
  (define-auto-insert "\\.html?$" "default.html")
  (define-auto-insert "\\.org" ["default.org" marty/autoinsert-yas-expand])
  (define-auto-insert "\\.sh" ["default.sh" marty/autoinsert-yas-expand])
  (define-auto-insert "\\.el" ["default.el" marty/autoinsert-yas-expand])
  (define-auto-insert "Roam/.+\\.org?$" ["defaultRoam.org" marty/autoinsert-yas-expand])
  (define-auto-insert "Blorg/snuffy-org/.+\\.org?$" ["snuffy-org.org" marty/autoinsert-yas-expand])
  (define-auto-insert "Sites/snuffy.org/.+\\.org?$" ["snuffy-org-posts.org" marty/autoinsert-yas-expand])
  (define-auto-insert "salt-master.+\\.org?$" ["salt-master.org" marty/autoinsert-yas-expand])
  (define-auto-insert "NSI-Documentation/[^/]+\\.org?$" ["NSI-Documentation.org" marty/autoinsert-yas-expand])
  (define-auto-insert "NSI-Documentation/.+/[^/]+\\.org?$" ["NSI-Documentation.org" marty/autoinsert-yas-expand])
  (define-auto-insert "NSI-Documentation/tipjar/[^/]+\\.org?$" ["NSI-Documentation-tipjar.org" marty/autoinsert-yas-expand])
  (define-auto-insert "NSI-Documentation/TVA/[^/]+\\.org?$" ["NSI-Documentation-TVA.org" marty/autoinsert-yas-expand])
  (define-auto-insert "NSI-Documentation/TVA/ScanReports/.+[^/]+\\.org?$" ["NSI-Documentation-TVA-scanreport.org" marty/autoinsert-yas-expand])
  (define-auto-insert "NSI-Documentation/Patching/.+[^/]+\\.org?$" ["NSI-Documentation-Patching-Notes.org" marty/autoinsert-yas-expand])
  (define-auto-insert "masons/[^/].+\\.org?$" ["masonsMeetingMinuets.org" marty/autoinsert-yas-expand])
  (define-auto-insert "daily/[^/].+\\.org?$" ["defaultRoamDaily.org" marty/autoinsert-yas-expand])
  (define-auto-insert "/[0-9]\\{8\\}.org$" ["defaultJournal.org" marty/autoinsert-yas-expand]))

;;;;; DIRED

(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; with dired-open plugin, you can launch external programs for certain extensions
;; for example, i set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))
;;;;; COMPANY

(after! company
  (setq company-idle-delay 0.5)
  (setq company-backends
        '(company-capf company-dabbrev company-files company-yasnippet)
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))

(use-package! company-box
  :after company
  :config
  (setq company-box-max-candidates 5))

(use-package! company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

;;;;; MAGIT

(after! magit
  (setq magit-commit-arguments '("--gpg-sign=7ACF5A8C6A106D1F561F1699E72D93984A1E2483")
        magit-rebase-arguments '("--autostash" "--gpg-sign=7ACF5A8C6A106D1F561F1699E72D93984A1E2483")
        magit-pull-arguments   '("--rebase" "--autostash" "--gpg-sign=7ACF5A8C6A106D1F561F1699E72D93984A1E2483"))
  (magit-define-popup-option 'magit-rebase-popup
    ?S "Sign using gpg" "--gpg-sign=" #'magit-read-gpg-secret-key)
  (setq magit-revision-show-gravatars '("^author:     " . "^commit:     ")))

;;;;; SPELL

(after! flyspell
  (setq ispell-personal-dictionary (expand-file-name "dictionary/personal" doom-private-dir))
  (setq flyspell-lazy-idle-seconds 0.5))

;;;;; TREEMACS

(after! treemacs
  (setq +treemacs-git-mode 'extended)
  (setq treemacs-width 30))

;;;; MODULES
;;;;; ACTIVITY WATCH MODE


(defun marty/startactivitywatchmode ()
  (interactive)
  (global-activity-watch-mode))

(use-package! activity-watch-mode
  :defer t
  :config
  (add-hook 'doom-first-buffer-hook #'marty/startactivitywatchmode))

;;;;; AGGRESSIVE INDENT

(use-package! aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook    #'aggressive-indent-mode)
  (add-hook 'php-mode-hook #'aggressive-indent-mode)
  (add-hook 'hy-mode-hook #'aggressive-indent-mode))

;;;;; I3 WINDOW MANAGER CONFIG
;; Syntax highlighting for i3 config
(use-package! i3wm-config-mode
      :defer t )

;;;;; JENKINS

(use-package! jenkinsfile-mode
:defer t )

;;;;; KHARD

(use-package! khardel
  :defer t )

;;;;; NGINX

(use-package! company-nginx
  :after nginx-mode
  :config (add-hook 'nginx-mode-hook (lambda () (add-to-list 'company-backends #'company-nginx))))

(use-package! nginx-mode
  :defer t)

;;;;; MUTT-MODE

(use-package! mutt-mode
  :defer t)

;;;;; OUTSHINE

(use-package! outshine
  :config
  (map! :map emacs-lisp-mode-map
        "TAB" #'outshine-cycle)
  (add-hook 'prog-mode-hook #'outline-minor-mode)
  (add-hook 'outline-minor-mode-hook #'outshine-mode)
  (defvar outline-minor-mode-prefix "\M-#"))

;;;;; PAPERLESS

(use-package! paperless
  :commands (paperless)
  :config (progn
            (custom-set-variables
             '(paperless-capture-directory "~/Nextcloud/Documents/INBOX")
             '(paperless-root-directory "~/Nextcloud/Documents")))
  (map! :after paperless
        :localleader
        :mode paperless-mode
        "d"  #'paperless-display
        "r"  #'paperless-rename
        "r"  #'paperless-scan-directories
        "f"  #'paperless-file
        "x"  #'paperless-execute))

;;;;; RAINBOW MODE

(use-package! rainbow-mode
  :hook (((css-mode scss-mode org-mode emacs-lisp-mode typescript-mode js-mode). rainbow-mode)))

;;;;; SALT MODE

(use-package! salt-mode
  :defer t)

;;;;; SYSTEMD MODE

(use-package! systemd
  :defer t
  :config
  (map! :map systemd-mode
        :localleader
        :prefix ("h" . "help")
        "d" #'systemd-doc-directives
        "o" #'systemd-doc-open))

;;;;; VLF

(use-package! vlf-setup
  :defer-incrementally  vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;;;;; WAKATIME

(defun marty/startwakatime ()
  (interactive)
  (setq wakatime-api-key (auth-source-pass-get 'secret "Application/wakatime/apikey"))
  (global-wakatime-mode))

(use-package! wakatime-mode
  :config
  (add-hook 'doom-first-buffer-hook  #'marty/startwakatime)
  (cond (IS-MAC
         (setq wakatime-cli-path "/usr/local/bin/wakatime-cli"))
        (t
          (setq wakatime-cli-path "/usr/bin/wakatime")))

;;;; LOAD

(load! "functions.el")
(load! "org-mode.el")
(load! "keybindings.el")
(load! "hydra.el")
(load! "mu4e.el")

;;; CUSTOM

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
