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

(setq auto-save-default t)                         ; nobody likes to loose work, i certainly don't
(setq confirm-kill-emacs nil)                      ; stop hounding me and quit
(setq display-time-24hr-format t)                  ; I wonder what this does
(setq evil-want-fine-undo t)                       ; by default while in insert all changes are one big blob. be more granular
(setq inhibit-compacting-font-caches t)
(setq password-cache-expiry nil)                   ; i can trust my computers ... can't i?
(setq read-process-output-max (* 1024 1024))
(setq scroll-margin 2)                             ; it's nice to maintain a little margin
(setq truncate-string-ellipsis "…")                ; unicode ellispis are nicer than "...", and also save /precious/ space
(setq undo-limit 80000000)                         ; raise undo-limit to 80mb
(setq warning-minimum-level :emergency)

(setq garbage-collection-messages nil)

(after! gcmh
  (setq gcmh-high-cons-threshold 67108864))  ; 33554432 32mb, or 67108864 64mb, or *maybe* 128mb, BUT NOT 512mb

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)  ; Make the scratch buffer start in lisp mode

(display-time-mode 1)                              ; enable time in the mode-line
(setq display-time-load-average nil)

(global-subword-mode 1)                            ; CamelCase and it makes refactoring slightly Essie

(set-window-buffer nil (current-buffer))
(setenv "zstd" "/usr/bin/zstd")

(add-to-list 'load-path "~/.config/doom/elisp")

(add-hook! 'doom-first-buffer-hook  #'marty/startwakatime)

;;;;; ON-SAVE

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            yaml-mode
            latex-mode))

;;;;; VTERM

(setq vterm-kill-buffer-on-exit t)
(setq vterm-always-compile-module t)               ; Always compile the vterm module
(setq vterm-timer-delay nil)                       ; Make Vterm snappier
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


(after! doom-themes
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t))

;;;;; MODELINE
(after! doom-modeline
  (setq auto-revert-check-vc-info t
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-enable-word-count t
        doom-modeline-github t
        doom-modeline-major-mode-color-icon (display-graphic-p)
        doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-mu4e t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-modal-icon nil
        doom-modeline-vcs-max-length 60)

  (set-face-attribute 'mode-line nil :family "Noto Sans" :height 100)
  (set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 100)

  (add-hook! 'doom-modeline-mode-hook
    (progn
      (set-face-attribute 'header-line nil
                          :background (face-background 'mode-line)
                          :foreground (face-foreground 'mode-line)
                          )))


  (doom-modeline-def-segment buffer-name
    "Display the current buffer's name, without any other information."
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-name)))

  (doom-modeline-def-segment pdf-icon
    "PDF icon from all-the-icons."
    (concat
     (doom-modeline-spc)
     (doom-modeline-icon 'octicon "file-pdf" nil nil
                         :face (if (doom-modeline--active)
                                   'all-the-icons-red
                                 'mode-line-inactive)
                         :v-adjust 0.02)))

  (defun doom-modeline-update-pdf-pages ()
    "Update PDF pages."
    (setq doom-modeline--pdf-pages
          (let ((current-page-str (number-to-string (eval `(pdf-view-current-page))))
                (total-page-str (number-to-string (pdf-cache-number-of-pages))))
            (concat
             (propertize
              (concat (make-string (- (length total-page-str) (length current-page-str)) ? )
                      " P" current-page-str)
              'face 'mode-line)
             (propertize (concat "/" total-page-str) 'face 'doom-modeline-buffer-minor-mode)))))

  (doom-modeline-def-segment pdf-pages
    "Display PDF pages."
    (if (doom-modeline--active) doom-modeline--pdf-pages
      (propertize doom-modeline--pdf-pages 'face 'mode-line-inactive)))

  (doom-modeline-def-modeline 'pdf
    '(bar window-number pdf-pages pdf-icon buffer-name)
    '(misc-info matches major-mode process vcs)))


;;;;; DASHBOARD

;; (setq doom-fallback-buffer-name "► Doom"
;;       +doom-dashboard-name "► Doom")

;; (custom-theme-set-faces! 'doom-one
;;   '(doom-dashboard-banner :foreground "red" :background "#000000" :weight bold)
;;   '(doom-dashboard-footer :inherit font-lock-constant-face)
;;   '(doom-dashboard-footer-icon :inherit all-the-icons-red)
;;   '(doom-dashboard-loaded :inherit font-lock-warning-face)
;;   '(doom-dashboard-menu-desc :inherit font-lock-string-face)
;;   '(doom-dashboard-menu-title :inherit font-lock-function-name-face))

;; ;; (setq fancy-splash-image (expand-file-name "banners/doom-emacs-slant-out-color.png" doom-private-dir))
;; (setq fancy-splash-image (expand-file-name "banners/smaller-cute-demon.png" doom-private-dir))

;; (setq dashboard-center-content nil) ;; set to 't' for centered content

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "\nKEYBINDINGS:\
\nFind file               (SPC .)     \
Open buffer list    (SPC b i)\
\nFind recent files       (SPC f r)   \
Open the eshell     (SPC e s)\
\nOpen dired file manager (SPC d d)   \
List of keybindings (SPC h b b)")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner (expand-file-name "banners/smaller-cute-demon.png" doom-private-dir))
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))

(setq doom-fallback-buffer "*dashboard*")

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
  (setq vertico-resize t))


;;;;; COMPANY

(after! company
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-backends '(company-capf company-dabbrev company-files company-yasnippet)
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))

(use-package! company-box
  :after company
  :config
  (setq company-box-max-candidates 5))

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(use-package! company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

(set-company-backend! '(text-mode markdown-mode gfm-mode)
  '(:seperate company-ispell company-files company-yasnippet))
(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))

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

;;;;; EROS

(setq eros-eval-result-prefix "⟹ ")

;;;;; MAGIT

(after! magit
  (setq magit-revision-show-gravatars '("^author:     " . "^commit:     ")))


;;;;; PROJECTILE

(after! projectile
  (setq projectile-indexing-method 'alien)
  (setq projectile-project-search-path '("~/Source" "~/.local/share"))
  (setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
  (defun projectile-ignored-project-function (filepath)
    "Return t if FILEPATH is within any of `projectile-ignored-projects'"
    (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects))))

;;;;; SPELL

(after! flyspell
  (setq ispell-personal-dictionary (expand-file-name "dictionary/personal" doom-private-dir))
  (setq flyspell-lazy-idle-seconds 0.5))

;;;;; TREEMACS

(after! treemacs
  (setq +treemacs-git-mode 'extended)
  (setq treemacs-width 30)
  (treemacs-load-theme 'Default))

;;;;; WHICHKEY

(setq which-key-idle-delay 1 ) ;; I need the help, I really do
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

;; tell which-key to behave
(setq which-key-use-C-h-commands t
      prefix-help-command #'which-key-C-h-dispatch)

(defadvice! fix-which-key-dispatcher-a (fn &rest args)
  :around #'which-key-C-h-dispatch
  (let ((keys (this-command-keys-vector)))
    (if (equal (elt keys (1- (length keys))) ?\?)
        (let ((keys (which-key--this-command-keys)))
          (embark-bindings (seq-take keys (1- (length keys)))))
      (apply fn args))))

;;;; MODULES
;;;;; AGGRESSIVE INDENT

(use-package! aggressive-indent
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook    #'aggressive-indent-mode)
  (add-hook 'php-mode-hook #'aggressive-indent-mode)
  (add-hook 'hy-mode-hook #'aggressive-indent-mode))

;;;;; BEACON

(beacon-mode 1)

;;;;; GRIP

(after! grip-mode)
  (setq grip-github-password (auth-source-pass-get 'secret "Application/github.com/emacs-token"))

;;;;; I3 WINDOW MANAGER CONFIG

;; Syntax highlighting for i3 config
(use-package! i3wm-config-mode
  :defer t )

;;;;; INFO-COLORS

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

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

;; my pitches getting all mixed up
(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)

;;;;; OUTSHINE

(use-package! outshine
  :hook (prog-mode . outshine-mode)
  :config
  (map! :map emacs-lisp-mode-map
        "TAB" #'outshine-cycle)
  (add-hook 'prog-mode-hook #'outline-minor-mode)
  (add-hook 'outline-minor-mode-hook #'outshine-mode)
  (defvar outline-minor-mode-prefix "\M-#"))

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
  (cond (IS-MAC (setq wakatime-cli-path "/usr/local/bin/wakatime-cli"))
        (IS-LINUX (setq wakatime-cli-path "/usr/bin/wakatime"))))


;;;; LOAD

(load! "+keybindings.el")
(load! "+functions.el")
(load! "+org-mode.el")
(load! "+hydra.el")
(load! "+mu4e.el")
(load! "+abbrev.el")

;;; CUSTOM

(setq-default custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
