;; $doomdir/config.el --- summary -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2021, marty buchaus, all rights reserved.
;; created:  1 november 2021
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Notes
;;  2021 10 12  added code from Stuff from  https://github.com/Artawower/.doom/blob/main/config.el#L308
;;

(defun native-comp-available-p () nil)             ; fixing issue with org-agenda with this function negation

(setq user-full-name "Marty Buchaus")
(setq user-mail-address "marty@dabuke.com")
(setq epg-gpg-program "/usr/bin/gpg")

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

;;;;; SERVER

(require 'server)
(when (not (server-running-p))
  (server-start))

;; (defun greedily-do-daemon-setup ()
;;   (require 'org)
;;   (when (require 'mu4e nil t)
;;     (setq mu4e-confirm-quit t)
;;     (setq +mu4e-lock-greedy t)
;;     (setq +mu4e-lock-relaxed t)
;;     (+mu4e-lock-add-watcher)
;;     (when (+mu4e-lock-available t)
;;       (mu4e~start))))

;; (when (daemonp)
;;   (add-hook 'emacs-startup-hook #'greedily-do-daemon-setup)
;;   (add-hook! 'server-after-make-frame-hook (switch-to-buffer +doom-dashboard-name)))

;;;;; EVIL-SETTINGS

(setq! evil-want-Y-yank-to-eol nil)

;;;;; FONTS

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

;;;;;; MIXED PITCH

;;(add-hook! 'org-mode-hook #'mixed-pitch-mode)

(defvar marty/mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Only use `mixed-pitch-mode' for given modes.")

(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode of `marty/mixed-pitch-modes'"
  (when (memq major-mode marty/mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook marty/mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))

(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)

;; additional colors
(setq +m-color-main "#61AFEF"
      +m-color-secondary "red")

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

;;;;; REGISTERS

;; These registers are manually added by calling this function for Rackspace
;; Patching
;;

(defun marty/set-patching-macro-registers ()
  "evil keyboard macros for patching,  running docker containers"
  (interactive)
  (evil-set-register ?e [?0 ?i ?* ?* ?* ?* ?* ?* ?  escape ?0])
  (evil-set-register ?b [?0 ?o escape ?0 ?i ?# ?+ ?e ?n ?d ?_ ?e ?x ?a ?m ?p ?l ?e escape ?0] )
  (evil-set-register ?t [?0 ?o ?i backspace ?# ?+ ?b ?e ?g ?i ?n ?_ ?e ?x ?a ?m ?p ?l ?e escape ?0]))

;;;; SPELLING

(use-package! flyspell
  :defer 7
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

;;;; KEYBINDINGS

(load! "keybindings.el")

;;;; DIRED

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

;;;; ORG MODE

(load! "org-mode.el")

;;;; FUNCTIONS

(load! "functions.el")

;;;; MU4E

(load! "mu4e.el")
