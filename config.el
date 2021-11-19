;;; $doomdir/config.el --- summary -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2021, marty buchaus, all rights reserved.
;; created:  1 November 2021
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code
;;;; Notes

;;  2021 11 18 Update clean Install and config
;;  2021 10 12  added code from Stuff from  https://github.com/Artawower/.doom/blob/main/config.el#L308

;;;; Global

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
(setq garbage-collection-messages t)

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)  ; Make the scratch buffer start in lisp mode

(display-time-mode 1)                              ; enable time in the mode-line

(global-subword-mode 1)                            ; CamelCase and it makes refactoring slightly Essie

(after! projectile
  (setq projectile-project-search-path '("~/Source")))
;; this doesn't seem to work to fix the doom doctor issue
;; (setq projectile-project-root-files-bottom-up (remove ".git" projectile-project-root-files-bottom-up)))

(set-window-buffer nil (current-buffer))
(setenv "zstd" "/usr/bin/zstd")

;;;;; VTERM

(setq vterm-kill-buffer-on-exit t)
(setq vterm-always-compile-module t)               ; Always compile the vterm module
(setq vterm-shell "/usr/bin/zsh")

;;;; UI
;;;;; SET FONTS
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 15 :weight 'regular )
      doom-variable-pitch-font (font-spec :family "Ubuntu" :style "Regular" :size 15 :weight 'regular)
      doom-unicode-font (font-spec :family "symbola" :size 14)
      doom-big-font (font-spec :family "DejaVu Sans Mono" :size 24))

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
        doom-themes-enable-italic t))

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

;;;;; LINE NUMBERS

(setq display-line-numbers-type 'relative)

;; remove numbers from these modes

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;;; Packages
;;;;; SPELLING

(use-package! flyspell
  :defer t
  :config
  ;; (setq ispell-program-name "aspell")
  ;; You could add extra option "--camel-case" for since Aspell 0.60.8
  ;; @see https://github.com/redguardtoo/emacs.d/issues/796
  ;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))
  (setq-default flyspell-prog-text-faces
                '(tree-sitter-hl-face:comment
                  tree-sitter-hl-face:doc
                  tree-sitter-hl-face:string
                  tree-sitter-hl-face:function
                  tree-sitter-hl-face:variable
                  tree-sitter-hl-face:type
                  tree-sitter-hl-face:method
                  tree-sitter-hl-face:function.method
                  tree-sitter-hl-face:function.special
                  tree-sitter-hl-face:attribute
                  font-lock-comment-face
                  font-lock-doc-face
                  font-lock-string-face
                  lsp-face-highlight-textual
                  default))


  (setq flyspell-lazy-idle-seconds 2)
  (setq ispell-personal-dictionary "~/.config/doom/dictionary/ispell_personal" )
  (setq spell-fu-directory "~/.config/doom/dictionary") ;; Please create this directory manually.
  (after! ispell
    (setq ispell-program-name "aspell"
          ;; Notice the lack of "--run-together"
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))
    (ispell-kill-ispell t))

  (defun flyspell-buffer-after-pdict-save (&rest _)
    (flyspell-buffer))

  (advice-add 'ispell-pdict-save :after #'flyspell-buffer-after-pdict-save))


(add-hook 'text-mode-hook 'flyspell-mode!)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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

(setq company-idle-delay 0.5)

;;;;; DASHBOARD

(use-package! dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "emacs is more than a text editor!")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.config/doom/banners/doom-emacs-slant-out-bw.png")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book"))))
;;;;; MAGIT
(use-package! magit
  :config
  (define-key transient-map        "q" 'transient-quit-one)
  (define-key transient-edit-map   "q" 'transient-quit-one)
  (define-key transient-sticky-map "q" 'transient-quit-seq)
  (setq magit-revision-show-gravatars '("^author:     " . "^commit:     ")))

;;;; MODULES
;;;;; AGGRESSIVE INDENT

(use-package! aggressive-indent
  :defer t
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
  :defer t
  :hook (emacs-lisp-mode . outshine-mode)
  :config
  (map! :after outshine
        :map emacs-lisp-mode-map
        "TAB" #'outshine-cycle)
  (add-hook 'emacs-lisp-mode-hook #'outshine-mode)
  (defvar outline-minor-mode-prefix "\M-#"))

;;;;; PAPERLESS

(use-package! paperless
  :defer t
  :init (require 'org-paperless)
  :config (progn
            (custom-set-variables
             '(paperless-capture-directory "~/nextcloud/documents/inbox/")
             '(paperless-root-directory "~/nextcloud/documents"))))

(after! paperless
  (map! :leader
        :prefix "a"
        "x"  #'paperless)
  (map! :after paperless
        :localleader
        :mode paperless-mode
        "d"  #'paperless-display
        "r"  #'paperless-rename
        "r"  #'paperless-scan-directories
        "f"  #'paperless-file
        "x"  #'paperless-execute))

;;;;; RAINBOW MODE

(use-package rainbow-mode
  :defer t
  :hook (((css-mode scss-mode org-mode emacs-lisp-mode typescript-mode js-mode). rainbow-mode)))

;;;;; SALT MODE

(use-package! salt-mode
  :defer t)

;;;;; SYSTEMD MODE

(use-package! systemd
  :defer t)

(map! :map systemd-mode
      :localleader
      :prefix ("h" . "help")
      "d" #'systemd-doc-directives
      "o" #'systemd-doc-open)

;;;;; VLF

(use-package! vlf-setup
  :defer-incrementally  vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;;;;; WAKATIME

(defun marty/startwakatime ()
  (interactive)
  (setq wakatime-api-key (auth-source-pass-get 'secret "Application/wakatime/apikey"))
  (global-wakatime-mode))

(use-package! wakatime-mode
  :defer t
  :config
  (add-hook 'doom-first-buffer-hook  #'marty/startwakatime)
  (setq wakatime-cli-path "/usr/bin/wakatime"))



;;;; LOAD
(load! "functions.el")
(load! "org-mode.el")
(load! "keybindings.el")
(load! "mu4e.el")
;;; CUSTOM

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
