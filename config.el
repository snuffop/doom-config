;;; $doomdir/config.el --- My Emacs Config File -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2022, marty buchaus, all rights reserved.
;; created:  1 November 2021
;;
;;;; Notes
;;
;;  * 2021 12 29 Updated the outshine use-packages with a hook to save 3 seconds on startup time
;;  * 2021 12 08 Modified and working for OSX
;;  * 2021 11 18 Update clean Install and config
;;  * 2021 10 12 added code from Stuff from  https://github.com/Artawower/.doom/blob/main/config.el#L308
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code
;;;; GLOBAL

(setq user-full-name "Marty Buchaus")
(setq user-mail-address "marty@dabuke.com")
(setq epg-gpg-program "/usr/bin/gpg2")

(setq-default delete-by-moving-to-trash t)
(setq-default enable-local-variables t)            ; allow for reading the local variables file
(setq-default left-margin-width 1)                 ; Define new widths
(setq-default right-margin-width 2)                ; Define new widths.
(setq-default window-combination-resize t)
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

(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))                        ; Display Battery

(set-window-buffer nil (current-buffer))
(setenv "zstd" "/usr/bin/zstd")

;;;;; VTERM

(setq vterm-kill-buffer-on-exit t)
(setq vterm-always-compile-module t)               ; Always compile the vterm module
(setq vterm-shell "/usr/bin/zsh")

;;;; UI
;;;;; SET FONTS

(cond (IS-MAC (setq doom-font (font-spec :family "DejaVuSansMono Nerd Font Mono" :size 13 )
                    doom-variable-pitch-font (font-spec :family "Ubuntu Nerd Font" :style "Regular" :size 13 :weight 'regular)))
      (IS-LINUX (setq doom-font (font-spec :family "DejaVuSansMono Nerd Font Mono" :size 14 :weight 'regular )
                      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15 )
                      doom-unicode-font (font-spec :family "symbola" :size 15)
                      doom-big-font (font-spec :family "Ubuntu" :size 20))))

;;;;; FACES

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(setq global-prettify-symbols-mode t)

(cond (IS-MAC
       (custom-set-faces!
         '(mode-line :family "firacode nerd font mono" :height 140)
         '(mode-line-inactive :family "firacode nerd font mono" :height 140)))
      (t
       (custom-set-faces!
         '(mode-line :family "Fira Sans" :height 105)
         '(mode-line-inactive :family "Fira Sans" :height 105))))

;;;;; THEME

(setq doom-theme 'doom-one )

(doom-themes-org-config)

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
  (setq doom-modeline-github t)
  (setq doom-modeline-mu4e t)
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

(setq +doom-dashboard-menu-sections
  '(("Reload last session"
    :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
    :when (cond ((require 'persp-mode nil t)
                  (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                ((require 'desktop nil t)
                  (file-exists-p (desktop-full-file-name))))
    :face (:inherit (doom-dashboard-menu-title bold))
    :action doom/quickload-session)
    ("Open org-agenda"
    :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
    :when (fboundp 'org-agenda)
    :action org-agenda)
    ("Recently opened files"
    :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
    :action recentf-open-files)
    ("Open project"
    :icon (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
    :action projectile-switch-project)
    ("Jump to bookmark"
    :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
    :action bookmark-jump)
    ("Open private configuration"
    :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
    :when (file-directory-p doom-private-dir)
    :action doom/open-private-config)
    ("Open documentation"
    :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
    :action doom/help)))

;;;;; LINE NUMBERS

(setq display-line-numbers-type 'relative)

;; remove numbers from these modes

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

;;;;; SYNTAX LISTS


 (add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-mode))
 (add-to-list 'auto-mode-alist '("\\.service.tmpl\\'" . systemd-mode))
 (add-to-list 'auto-mode-alist '("\\.timer\\'" . systemd-mode))
 (add-to-list 'auto-mode-alist '("\\.timer.tmpl\\'" . systemd-mode))
 (add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
 (add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
 (add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
 (add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
 (add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
 (add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
 (add-to-list 'auto-mode-alist '("\\.netdev\\'" . conf-unix-mode))
 (add-to-list 'auto-mode-alist '("\\.network\\'" . conf-unix-mode))
 (add-to-list 'auto-mode-alist '("\\.link\\'" . conf-unix-mode))

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
  (define-auto-insert "Joyent/reports/bullets/.+[^/]+\\.org?$" ["Joyent-Weekly-Bullets.org" marty/autoinsert-yas-expand])
  (define-auto-insert "masons/[^/].+\\.org?$" ["masonsMeetingMinuets.org" marty/autoinsert-yas-expand])
  (define-auto-insert "daily/[^/].+\\.org?$" ["defaultRoamDaily.org" marty/autoinsert-yas-expand])
  (define-auto-insert "/[0-9]\\{8\\}.org$" ["defaultJournal.org" marty/autoinsert-yas-expand]))

;;;;; COMPLETION
;;;;;; VERTICO

(after! vertico
  (vertico-reverse-mode 1)
  (setq vertico-resize t)
)

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

;;;;; CONSULT

(after! consult
  (evil-collection-init 'consult))

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

;;;;; MAGIT

(after! magit
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
;;;;; AGGRESSIVE INDENT

(use-package! aggressive-indent
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook    #'aggressive-indent-mode)
  (add-hook 'php-mode-hook #'aggressive-indent-mode)
  (add-hook 'hy-mode-hook #'aggressive-indent-mode))

;;;;; CHEZMOI

(use-package! chezmoi
  :defer t
  :config
  (map! :leader
        :prefix "fz"
          "s" #'chezmoi-write
          "g" #'chezmoi-magit-status
          "d" #'chezmoi-diff
          "e" #'chezmoi-ediff
          "f" #'chezmoi-find
          "i" #'chezmoi-write-files-from-target
          "o" #'chezmoi-open-target))

;;;;; Ebuku

(use-package! ebuku
  :defer t
  :config
  (evil-collection-init 'ebuku))

;;;;; HYPERBOLE
;; OMG this is Amazing

(use-package! hyperbole
  :after org
  :config
  (add-hook 'doom-first-buffer-hook  #'hyperbole-mode)
  (setq hyrolo-file-list (cons "~/.rolo.otl" (cddr (directory-files "~/Nextcloud/Notes/org" t))))
  )


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

;;;;; MIXED-PITCH

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-face 'variable-pitch))

;;;;; MUTT-MODE

(use-package! mutt-mode
  :defer t)

;;;;; OUTSHINE

(use-package! outshine
  :hook (prog-mode . outshine-mode)
  :config
  (map! :map emacs-lisp-mode-map
        "TAB" #'outshine-cycle)
  (add-hook 'prog-mode-hook #'outline-minor-mode)
  (add-hook 'outline-minor-mode-hook #'outshine-mode)
  (defvar outline-minor-mode-prefix "\M-#"))

;;;;; SALT MODE

(use-package! salt-mode
  :defer t)

;;;;; SYSTEMD MODE

(use-package! systemd
  :defer t
  :mode "\\.service\\'"
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
  (cond (IS-MAC (setq wakatime-cli-path "/usr/local/bin/wakatime-cli"))
        (IS-LINUX (setq wakatime-cli-path "/usr/bin/wakatime"))))

;;;; LOAD

(load! "+keybindings.el")
(load! "+functions.el")
(load! "+org-mode.el")
(load! "+hydra.el")
(load! "+mu4e.el")

;;; CUSTOM

(setq-default custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
