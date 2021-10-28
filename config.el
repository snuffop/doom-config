;; config.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2021, Marty Buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes
;;  2021 10 12  added code from Stuff from  https://github.com/Artawower/.doom/blob/main/config.el#L308
;;
;;; CODE
;;;; GLOBAL

(setq user-full-name "Marty Buchaus")
(setq user-mail-address "marty@dabuke.com")
(setq epg-gpg-program "/usr/bin/gpg")

(setq-default enable-local-variables t)            ; allow for reading the local variables file
(setq-default window-combination-resize t)
(setq-default left-margin-width 1 right-margin-width 2) ; Define new widths.
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

(display-time-mode 1)                             ; enable time in the mode-line

(global-subword-mode 1)                            ; CamelCase and it makes refactoring slightly Essie

(after! projectile
  (setq projectile-project-search-path '("~/Source")))
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

(defun greedily-do-daemon-setup ()
  (require 'org)
  (when (require 'mu4e nil t)
    (setq mu4e-confirm-quit t)
    (setq +mu4e-lock-greedy t)
    (setq +mu4e-lock-relaxed t)
    (+mu4e-lock-add-watcher)
    (when (+mu4e-lock-available t)
      (mu4e~start))))

(when (daemonp)
  (add-hook 'emacs-startup-hook #'greedily-do-daemon-setup)
  (add-hook! 'server-after-make-frame-hook (switch-to-buffer +doom-dashboard-name)))

;;;;; EVIL-SETTINGS

(setq! evil-want-Y-yank-to-eol nil)

;;;; UI
;;;;; FONTS

(setq doom-font (font-spec :family "FiraCode" :size 14 :weight 'regular )
      doom-variable-pitch-font (font-spec :family "Firacode" :style "Regular" :size 14 :weight 'regular)
      doom-unicode-font (font-spec :family "symbola" :size 14)
      doom-big-font (font-spec :family "FiraCode" :size 24))


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

;;; MODULES
;;;; ACTIVITY WATCH MODE

(defun marty/startactivitywatchmode ()
  (interactive)
  (global-activity-watch-mode))

(use-package! activity-watch-mode
  :config
  (add-hook 'doom-first-buffer-hook #'marty/startactivitywatchmode))

;;;; AGGRESSIVE INDENT

(use-package! aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook    #'aggressive-indent-mode)
  (add-hook 'php-mode-hook #'aggressive-indent-mode)
  (add-hook 'hy-mode-hook #'aggressive-indent-mode))

(global-aggressive-indent-mode 1)

(use-package! alert)

(use-package! all-the-icons-completion)
(all-the-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

;;;; AUTOINSERT

;;;;; FUNCTIONS

(after! autoinsert
  (defun marty/autoinsert-yas-expand ()
    "This is used by the autoinsert package to grab and expand
templates into newly created files"
    (let ((template ( buffer-string )))
      (delete-region (point-min) (point-max))
      (yas-expand-snippet template)
      (evil-insert-state))))

(use-package! autoinsert
  :init (progn
          (setq auto-insert-query nil)
          (setq auto-insert-directory "~/.config/doom/templates")
          (add-hook 'find-file-hook 'auto-insert)
          (auto-insert-mode 1))
  :config (progn
            (define-auto-insert "\\.html?$" "default.html")
            ;; (define-auto-insert "\\.org" ["default.org" marty/autoinsert-yas-expand]) ;; disabled in favor of roam capture templates
            (define-auto-insert "\\.sh" ["default.sh" marty/autoinsert-yas-expand])
            (define-auto-insert "\\.el" ["default.el" marty/autoinsert-yas-expand])
            (define-auto-insert "blorg/snuffy-org/.+\\.org?$" ["snuffy-org.org" marty/autoinsert-yas-expand])
            (define-auto-insert "sites/snuffy.org/.+\\.org?$" ["snuffy-org-posts.org" marty/autoinsert-yas-expand])
            (define-auto-insert "salt-master.+\\.org?$" ["salt-master.org" marty/autoinsert-yas-expand])
            (define-auto-insert "NSI-Documentation/[^/]+\\.org?$" ["NSI-Documentation.org" marty/autoinsert-yas-expand])
            (define-auto-insert "NSI-Documentation/.+/[^/]+\\.org?$" ["NSI-Documentation.org" marty/autoinsert-yas-expand])
            (define-auto-insert "NSI-Documentation/tipjar/[^/]+\\.org?$" ["NSI-Documentation-tipjar.org" marty/autoinsert-yas-expand])
            (define-auto-insert "NSI-Documentation/TVA/[^/]+\\.org?$" ["NSI-Documentation-TVA.org" marty/autoinsert-yas-expand])
            (define-auto-insert "NSI-Documentation/TVA/ScanReports/.+[^/]+\\.org?$" ["NSI-Documentation-TVA-scanreport.org" marty/autoinsert-yas-expand])
            (define-auto-insert "NSI-Documentation/Patching/.+[^/]+\\.org?$" ["NSI-Documentation-Patching-Notes.org" marty/autoinsert-yas-expand])
            (define-auto-insert "masons/[^/].+\\.org?$" ["masonsMeetingMinuets.org" marty/autoinsert-yas-expand])
            (define-auto-insert "daily/[^/].+\\.org?$" ["defaultRoamDaily.org" marty/autoinsert-yas-expand])
            (define-auto-insert "/[0-9]\\{8\\}.org$" ["defaultJournal.org" marty/autoinsert-yas-expand])))

;;;; BROWSE-KILL-RING

(use-package! browse-kill-ring
  :config
  (progn
    (defadvice yank-pop (around kill-ring-browse-maybe (arg))
      "if last action was not a yank, run `browse-kill-ring' instead."
      (interactive "p")
      (if (not (eq last-command 'yank))
          (browse-kill-ring)
        (barf-if-buffer-read-only)
        ad-do-it))
    (ad-activate 'yank-pop))

  (map! :leader
      ;;; <leader> k --- application
        (:prefix-map ("k" . "kill ring")
         "k"  #'browse-kill-ring
         "i"  #'browse-kill-ring-append-insert)))

;;;; COMPANY

(setq company-idle-delay 0.5)

(defun my-setup-tabnine ()
  (interactive)
  (setq-local company-backends '(company-tabnine :separate company-capf)))

;; Only tabnine
(defun my-setup-tabnine-2 ()
  (interactive)
  (setq-local company-backends '(company-tabnine)))

(defun my-setup-tabnine-3 ()
  (interactive)
  (setq-local +lsp-company-backends '((company-capf)))
  (setq-local company-backends '((company-capf))))

(my-setup-tabnine)

;; Autocomplete with AI
(use-package! company-tabnine
  :after (company lsp)
  :when (featurep! :completion company)
  :config
  (setq company-idle-delay 0.1)
  (setq company-show-numbers nil)
  (setq company-minimum-prefix-length 1)
  (setq company-tabnine-show-annotation t)
  (setq company-dabbrev-char-regexp "[A-z:-]"))

;;;; COUNSEL TRAMP

(use-package! counsel-tramp
  :after tramp
  :config (progn
            (defadvice projectile-project-root (around ignore-remote first activate)
              (unless (file-remote-p default-directory) ad-do-it))

            (defvar disable-tramp-backups '(all))
            (setenv "shell" "/bin/bash")

            (setq tramp-default-method "sshx")
            (setq remote-file-name-inhibit-cache nil)
            (setq tramp-completion-reread-directory-timeout nil)
            (setq counsel-tramp-control-master t)

            (setq vc-ignore-dir-regexp
                  (format "\\(%s\\)\\|\\(%s\\)"
                          vc-ignore-dir-regexp
                          tramp-file-name-regexp)))

  (add-hook 'counsel-tramp-pre-command-hook
            #'(lambda () (global-aggressive-indent-mode 0)
                (projectile-mode 0)
                (editorconfig-mode 0)))

  (add-hook 'counsel-tramp-quit-hook
            #'(lambda () (global-aggressive-indent-mode 1)
                (projectile-mode 1)
                (editorconfig-mode 1)))

;;;;;; list of hosts
  (after! tramp
    (setq counsel-tramp-custom-connections
          '(
            /sshx:appgate:/home/marty
            /sshx:appgate|sudo:appgate:/
            /sshx:archstation.home.snuffy.org:/home/marty
            /sshx:archstation.home.snuffy.org:/
            /sshx:d-proxy-1.iad3.nsi.rackspace.com:/
            /sshx:virtarch.home.snuffy.org:/
            /sshx:virtarch.home.snuffy.org|sudo:virtarch.home.snuffy.org:/
            /sshx:danas.home.snuffy.org:/
            /sshx:daplex.home.snuffy.org:/
            /sshx:daplex.home.snuffy.org|sudo:daplex.home.snuffy.org:/
            /sshx:l1.dabuke.com:/
            /sshx:l1.dabuke.com|sudo.l1.dabuke.com:/
            /sshx:root@l2.dabuke.com:/
            /sshx:marty@l2.dabuke.com:/home/marty/
            /sshx:nextcloud@l2.dabuke.com:/home/nextcloud/
            /sshx:mail.dabuke.com:/
            /sshx:mail.dabuke.com|sudo:mail.dabuke.com:/
            /sshx:macpro.home.snuffy.org:/
            /sshx:macpro.home.snuffy.org|sudo:macpro.home.snuffy.org:/
            /sshx:nextcloud.home.snuffy.org:/
            /sshx:nexthost.home.snuffy.org:/
            /sshx:kali:/
            /sshx:kali|sudo:kali:/
            /sshx:ofmasons@l1.dabuke.com:/
            /sshx:radhits.net:/
            /sshx:radhits.net|sudo:radhits.net:/
            ;; rs
            /sshx:a-backup-1.ord1.nsi.rackspace.com|sudo:a-backup-1.ord1.nsi.rackspace.com:/
            /sshx:a-bastion-1.dfw3.nsi.rackspace.com|sudo:a-bastion-1.dfw3.nsi.rackspace.com:/
            /sshx:a-bastion-1.ord1.nsi.rackspace.com|sudo:a-bastion-1.ord1.nsi.rackspace.com:/
            /sshx:a-bastion-2.ord1.nsi.rackspace.com|sudo:a-bastion-2.ord1.nsi.rackspace.com:/
            /sshx:a-datastore-1.dfw3.nsi.rackspace.com|sudo:a-datastore-1.dfw3.nsi.rackspace.com:/
            /sshx:a-datastore-1.ord1.nsi.rackspace.com|sudo:a-datastore-1.ord1.nsi.rackspace.com:/
            /sshx:a-datastore-2.dfw3.nsi.rackspace.com|sudo:a-datastore-2.dfw3.nsi.rackspace.com:/
            /sshx:a-datastore-2.ord1.nsi.rackspace.com|sudo:a-datastore-2.ord1.nsi.rackspace.com:/
            /sshx:a-docker-netapi-1.dfw1.nsi.rackspace.com|sudo:a-docker-netapi-1.dfw1.nsi.rackspace.com:/
            /sshx:a-docker-netapi-1.hkg1.nsi.rackspace.com|sudo:a-docker-netapi-1.hkg1.nsi.rackspace.com:/
            /sshx:a-docker-netapi-1.iad3.nsi.rackspace.com|sudo:a-docker-netapi-1.iad3.nsi.rackspace.com:/
            /sshx:a-docker-netapi-1.lon3.nsi.rackspace.com|sudo:a-docker-netapi-1.lon3.nsi.rackspace.com:/
            /sshx:a-docker-netapi-1.ord1.nsi.rackspace.com|sudo:a-docker-netapi-1.ord1.nsi.rackspace.com:/
            /sshx:a-docker-netapi-1.syd2.nsi.rackspace.com|sudo:a-docker-netapi-1.syd2.nsi.rackspace.com:/
            /sshx:a-docker-netapi-2.dfw1.nsi.rackspace.com|sudo:a-docker-netapi-2.dfw1.nsi.rackspace.com:/
            /sshx:a-docker-netapi-2.hkg1.nsi.rackspace.com|sudo:a-docker-netapi-2.hkg1.nsi.rackspace.com:/
            /sshx:a-docker-netapi-2.iad3.nsi.rackspace.com|sudo:a-docker-netapi-2.iad3.nsi.rackspace.com:/
            /sshx:a-docker-netapi-2.lon3.nsi.rackspace.com|sudo:a-docker-netapi-2.lon3.nsi.rackspace.com:/
            /sshx:a-docker-netapi-2.ord1.nsi.rackspace.com|sudo:a-docker-netapi-2.ord1.nsi.rackspace.com:/
            /sshx:a-docker-netapi-2.syd2.nsi.rackspace.com|sudo:a-docker-netapi-2.syd2.nsi.rackspace.com:/
            /sshx:a-docker-registry-2.ord1.nsi.rackspace.com|sudo:a-docker-registry-2.ord1.nsi.rackspace.com:/
            /sshx:a-docker-services-1.dfw1.nsi.rackspace.com|sudo:a-docker-services-1.dfw1.nsi.rackspace.com:/
            /sshx:a-docker-services-1.hkg1.nsi.rackspace.com|sudo:a-docker-services-1.hkg1.nsi.rackspace.com:/
            /sshx:a-docker-services-1.iad3.nsi.rackspace.com|sudo:a-docker-services-1.iad3.nsi.rackspace.com:/
            /sshx:a-docker-services-1.lon3.nsi.rackspace.com|sudo:a-docker-services-1.lon3.nsi.rackspace.com:/
            /sshx:a-docker-services-1.ord1.nsi.rackspace.com|sudo:a-docker-services-1.ord1.nsi.rackspace.com:/
            /sshx:a-docker-services-1.syd2.nsi.rackspace.com|sudo:a-docker-services-1.syd2.nsi.rackspace.com:/
            /sshx:a-docker-services-2.dfw1.nsi.rackspace.com|sudo:a-docker-services-2.dfw1.nsi.rackspace.com:/
            /sshx:a-docker-services-2.hkg1.nsi.rackspace.com|sudo:a-docker-services-2.hkg1.nsi.rackspace.com:/
            /sshx:a-docker-services-2.iad3.nsi.rackspace.com|sudo:a-docker-services-2.iad3.nsi.rackspace.com:/
            /sshx:a-docker-services-2.lon3.nsi.rackspace.com|sudo:a-docker-services-2.lon3.nsi.rackspace.com:/
            /sshx:a-docker-services-2.ord1.nsi.rackspace.com|sudo:a-docker-services-2.ord1.nsi.rackspace.com:/
            /sshx:a-docker-services-2.syd2.nsi.rackspace.com|sudo:a-docker-services-2.syd2.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-1.dfw1.nsi.rackspace.com|sudo:a-docker-sshapi-1.dfw1.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-1.hkg1.nsi.rackspace.com|sudo:a-docker-sshapi-1.hkg1.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-1.iad3.nsi.rackspace.com|sudo:a-docker-sshapi-1.iad3.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-1.lon3.nsi.rackspace.com|sudo:a-docker-sshapi-1.lon3.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-1.ord1.nsi.rackspace.com|sudo:a-docker-sshapi-1.ord1.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-1.syd2.nsi.rackspace.com|sudo:a-docker-sshapi-1.syd2.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-2.dfw1.nsi.rackspace.com|sudo:a-docker-sshapi-2.dfw1.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-2.hkg1.nsi.rackspace.com|sudo:a-docker-sshapi-2.hkg1.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-2.iad3.nsi.rackspace.com|sudo:a-docker-sshapi-2.iad3.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-2.lon3.nsi.rackspace.com|sudo:a-docker-sshapi-2.lon3.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-2.ord1.nsi.rackspace.com|sudo:a-docker-sshapi-2.ord1.nsi.rackspace.com:/
            /sshx:a-docker-sshapi-2.syd2.nsi.rackspace.com|sudo:a-docker-sshapi-2.syd2.nsi.rackspace.com:/
            /sshx:a-docker-swarm-1.nglab.nsi.rackspace.com|sudo:a-docker-swarm-1.nglab.nsi.rackspace.com:/
            /sshx:a-docker-swarm-2.nglab.nsi.rackspace.com|sudo:a-docker-swarm-2.nglab.nsi.rackspace.com:/
            /sshx:a-docker-swarm-3.nglab.nsi.rackspace.com|sudo:a-docker-swarm-3.nglab.nsi.rackspace.com:/
            /sshx:a-docker-swarm-4.nglab.nsi.rackspace.com|sudo:a-docker-swarm-4.nglab.nsi.rackspace.com:/
            /sshx:a-docker-swarm-5.nglab.nsi.rackspace.com|sudo:a-docker-swarm-5.nglab.nsi.rackspace.com:/
            /sshx:a-iso-mirror-1.dfw1.nsi.rackspace.com|sudo:a-iso-mirror-1.dfw1.nsi.rackspace.com:/
            /sshx:a-iso-mirror-1.hkg1.nsi.rackspace.com|sudo:a-iso-mirror-1.hkg1.nsi.rackspace.com:/
            /sshx:a-iso-mirror-1.iad3.nsi.rackspace.com|sudo:a-iso-mirror-1.iad3.nsi.rackspace.com:/
            /sshx:a-iso-mirror-1.lon3.nsi.rackspace.com|sudo:a-iso-mirror-1.lon3.nsi.rackspace.com:/
            /sshx:a-iso-mirror-1.ord1.nsi.rackspace.com|sudo:a-iso-mirror-1.ord1.nsi.rackspace.com:/
            /sshx:a-iso-mirror-1.syd2.nsi.rackspace.com|sudo:a-iso-mirror-1.syd2.nsi.rackspace.com:/
            /sshx:a-jenkins-1.dfw3.nsi.rackspace.com|sudo:a-jenkins-1.dfw3.nsi.rackspace.com:/
            /sshx:a-jenkins-1.ord1.nsi.rackspace.com|sudo:a-jenkins-1.ord1.nsi.rackspace.com:/
            /sshx:a-nagios-1.dfw1.nsi.rackspace.com|sudo:a-nagios-1.dfw1.nsi.rackspace.com:/
            /sshx:a-nagios-1.iad3.nsi.rackspace.com|sudo:a-nagios-1.iad3.nsi.rackspace.com:/
            /sshx:a-nagios-1.lon3.nsi.rackspace.com|sudo:a-nagios-1.lon3.nsi.rackspace.com:/
            /sshx:a-redis-1.dfw1.nsi.rackspace.com|sudo:a-redis-1.dfw1.nsi.rackspace.com:/
            /sshx:a-redis-1.hkg1.nsi.rackspace.com|sudo:a-redis-1.hkg1.nsi.rackspace.com:/
            /sshx:a-redis-1.iad3.nsi.rackspace.com|sudo:a-redis-1.iad3.nsi.rackspace.com:/
            /sshx:a-redis-1.lon3.nsi.rackspace.com|sudo:a-redis-1.lon3.nsi.rackspace.com:/
            /sshx:a-redis-1.ord1.nsi.rackspace.com|sudo:a-redis-1.ord1.nsi.rackspace.com:/
            /sshx:a-redis-1.syd2.nsi.rackspace.com|sudo:a-redis-1.syd2.nsi.rackspace.com:/
            /sshx:a-redis-2.dfw1.nsi.rackspace.com|sudo:a-redis-2.dfw1.nsi.rackspace.com:/
            /sshx:a-redis-2.hkg1.nsi.rackspace.com|sudo:a-redis-2.hkg1.nsi.rackspace.com:/
            /sshx:a-redis-2.iad3.nsi.rackspace.com|sudo:a-redis-2.iad3.nsi.rackspace.com:/
            /sshx:a-redis-2.lon3.nsi.rackspace.com|sudo:a-redis-2.lon3.nsi.rackspace.com:/
            /sshx:a-redis-2.ord1.nsi.rackspace.com|sudo:a-redis-2.ord1.nsi.rackspace.com:/
            /sshx:a-redis-2.syd2.nsi.rackspace.com|sudo:a-redis-2.syd2.nsi.rackspace.com:/
            /sshx:a-salt-master-1.ord1.nsi.rackspace.com|sudo:a-salt-master-1.ord1.nsi.rackspace.com:/
            /sshx:a-salt-master-2.ord1.nsi.rackspace.com|sudo:a-salt-master-2.ord1.nsi.rackspace.com:/
            /sshx:a-salt-syndic-1.dfw1.nsi.rackspace.com|sudo:a-salt-syndic-1.dfw1.nsi.rackspace.com:/
            /sshx:a-salt-syndic-1.hkg1.nsi.rackspace.com|sudo:a-salt-syndic-1.hkg1.nsi.rackspace.com:/
            /sshx:a-salt-syndic-1.iad3.nsi.rackspace.com|sudo:a-salt-syndic-1.iad3.nsi.rackspace.com:/
            /sshx:a-salt-syndic-1.lon3.nsi.rackspace.com|sudo:a-salt-syndic-1.lon3.nsi.rackspace.com:/
            /sshx:a-salt-syndic-1.nglab.nsi.rackspace.com|sudo:a-salt-syndic-1.nglab.nsi.rackspace.com:/
            /sshx:a-salt-syndic-1.syd2.nsi.rackspace.com|sudo:a-salt-syndic-1.syd2.nsi.rackspace.com:/
            /sshx:a-scriptrunner-1.dfw3.nsi.rackspace.com|sudo:a-scriptrunner-1.dfw3.nsi.rackspace.com:/
            /sshx:a-scriptrunner-1.ord1.nsi.rackspace.com|sudo:a-scriptrunner-1.ord1.nsi.rackspace.com:/
            /sshx:a-scriptrunner-2.dfw3.nsi.rackspace.com|sudo:a-scriptrunner-2.dfw3.nsi.rackspace.com:/
            /sshx:a-scriptrunner-2.ord1.nsi.rackspace.com|sudo:a-scriptrunner-2.ord1.nsi.rackspace.com:/
            /sshx:a-syslog-1.dfw1.nsi.rackspace.com|sudo:a-syslog-1.dfw1.nsi.rackspace.com:/
            /sshx:a-syslog-1.hkg1.nsi.rackspace.com|sudo:a-syslog-1.hkg1.nsi.rackspace.com:/
            /sshx:a-syslog-1.iad3.nsi.rackspace.com|sudo:a-syslog-1.iad3.nsi.rackspace.com:/
            /sshx:a-syslog-1.lon3.nsi.rackspace.com|sudo:a-syslog-1.lon3.nsi.rackspace.com:/
            /sshx:a-syslog-1.ord1.nsi.rackspace.com|sudo:a-syslog-1.ord1.nsi.rackspace.com:/
            /sshx:a-syslog-1.syd2.nsi.rackspace.com|sudo:a-syslog-1.syd2.nsi.rackspace.com:/
            /sshx:a-syslog-2.dfw1.nsi.rackspace.com|sudo:a-syslog-2.dfw1.nsi.rackspace.com:/
            /sshx:a-syslog-2.hkg1.nsi.rackspace.com|sudo:a-syslog-2.hkg1.nsi.rackspace.com:/
            /sshx:a-syslog-2.iad3.nsi.rackspace.com|sudo:a-syslog-2.iad3.nsi.rackspace.com:/
            /sshx:a-syslog-2.lon3.nsi.rackspace.com|sudo:a-syslog-2.lon3.nsi.rackspace.com:/
            /sshx:a-syslog-2.ord1.nsi.rackspace.com|sudo:a-syslog-2.ord1.nsi.rackspace.com:/
            /sshx:a-syslog-2.syd2.nsi.rackspace.com|sudo:a-syslog-2.syd2.nsi.rackspace.com:/
            /sshx:a-websvcs-1.dfw1.nsi.rackspace.com|sudo:a-websvcs-1.dfw1.nsi.rackspace.com:/
            /sshx:a-websvcs-1.ord1.nsi.rackspace.com|sudo:a-websvcs-1.ord1.nsi.rackspace.com:/
            /sshx:a-websvcs-2.dfw1.nsi.rackspace.com|sudo:a-websvcs-2.dfw1.nsi.rackspace.com:/
            /sshx:a-websvcs-2.ord1.nsi.rackspace.com|sudo:a-websvcs-2.ord1.nsi.rackspace.com:/
            /sshx:a-websvcs-3.dfw1.nsi.rackspace.com|sudo:a-websvcs-3.dfw1.nsi.rackspace.com:/
            /sshx:a-zabbix-proxy-1.dfw3.nsi.rackspace.com|sudo:a-zabbix-proxy-1.dfw3.nsi.rackspace.com:/
            /sshx:a-zabbix-proxy-1.hkg1.nsi.rackspace.com|sudo:a-zabbix-proxy-1.hkg1.nsi.rackspace.com:/
            /sshx:a-zabbix-proxy-1.iad3.nsi.rackspace.com|sudo:a-zabbix-proxy-1.iad3.nsi.rackspace.com:/
            /sshx:a-zabbix-proxy-1.lon3.nsi.rackspace.com|sudo:a-zabbix-proxy-1.lon3.nsi.rackspace.com:/
            /sshx:rack@a-zabbix-proxy-1.nglab.nsi.rackspace.com|sudo:a-zabbix-proxy-1.nglab.nsi.rackspace.com:/
            /sshx:a-zabbix-proxy-1.ord1.nsi.rackspace.com|sudo:a-zabbix-proxy-1.ord1.nsi.rackspace.com:/
            /sshx:a-zabbix-proxy-1.syd2.nsi.rackspace.com|sudo:a-zabbix-proxy-1.syd2.nsi.rackspace.com:/
            /sshx:d-salt-master-1.iad3.nsi.rackspace.com|sudo:d-salt-master-1.iad3.nsi.rackspace.com:/
            /sshx:s-jenkins-uk-1.lon3.nsi.rackspace.com|sudo:s-jenkins-uk-1.lon3.nsi.rackspace.com:/
            ))))
;; end of  list of hosts

;;;; DASHBOARD

(use-package! dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "emacs is more than a text editor!")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.config/doom/banners/doom-emacs-slant-out-bw.png")  ;; use custom image as banner
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

;;;; DOCKER COMPOSE

(use-package! docker-compose-mode
  :defer 6)

;;;; DOCKER

(use-package! dockerfile-mode
  :defer 6)

;;;; EBUKU
;;
;; Add the E-buku management mode for buku

(use-package! ebuku
  :init
  (evil-collection-init 'ebuku)
  :config
  (evil-collection-ebuku-setup)
  (map! :leader
        :prefix "a"
        "b" #'ebuku))

;;;; ELFEED PROTOCOL
(use-package! elfeed-protocol
  :config
  (setq elfeed-feeds '(("owncloud+https://marty@nextcloud.dabuke.com"
                        :password (auth-source-pass-get 'secret "Login/nextcloud.dabuke.com/marty"))))
  (map! :leader
        :prefix "a"
        "e" #'elfeed)
  (elfeed-protocol-enable))

;;;; ELISP

;; (use-package! emacs-lisp-
;;   :defer 4
;;   :bind (("C-c o" . #'outline-cycle)
;;          ("C-c r" . #'outline-show-all)
;;          ("C-c m" . #'outline-hide-body)
;;          ("C-c ]" . #'outline-next-heading)
;;          ("C-c [" . #'outline-previous-heading)
;;          ("C-c c" . #'counsel-outline)
;;          ("C-c e" . #'outline-hide-entry)
;;          ("C-c t" . #'outline-toggle-children)
;;          ("C-c b" . #'outline-cycle-buffer)))

;;;; GIT
;;;;; MAGIT

(use-package! magit
  :defer 0.3
  :config
  (define-key transient-map        "q" 'transient-quit-one)
  (define-key transient-edit-map   "q" 'transient-quit-one)
  (define-key transient-sticky-map "q" 'transient-quit-seq)
  (setq magit-revision-show-gravatars '("^author:     " . "^commit:     ")))

;;;;; GIT-GUTTER

(use-package! git-gutter
  :defer 3
  :init
  (global-git-gutter-mode)
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk))

;;;; Hydra

(use-package! hydra
  :defer 8)

;;;; I3 WINDOW MANAGER CONFIG
;; Syntax highlighting for i3 config
(use-package! i3wm-config-mode)

;;;; INFO PAGES

(use-package! info-colors
  :after info
  :commands (info-colors-fontify-node)
  :hook (Info-selection . info-colors-fontify-node))

;;;; JENKINS

(use-package! jenkinsfile-mode)

;;;; KHARD

(use-package! khardel)

;;;; KHALEL

(use-package! khalel
  :config
  (progn
    (setq khalel-khal-command "/usr/bin/khal")
    (setq khalel-vdirsyncer-command "vdirsyncer")
    (setq khalel-default-calendar "personal")
    (setq khalel-import-org-file (concat org-directory "Calendar.org"))
    (setq khalel-import-time-delta "365d")
    (setq khalel-import-org-file-confirm-overwrite nil)
    (setq khalel-capture-key "e")
    (khalel-add-capture-template)
    ))

;;;; LEDGER

(setq ledger-post-amount-alignment-column 69)

;;;; LSP

(use-package! lsp
  :defer 0.1
  ;; TIDE check, less laggi?
  ;; :hook (((go-mode scss-mode css-mode web-mode ng2-html-mode ng2-ts-mode python-mode) . lsp-deferred))
  :hook (((go-mode scss-mode css-mode js-mode typescript-mode vue-mode web-mode ng2-html-mode ng2-ts-mode python-mode) . lsp-deferred))

  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.3)
  (lsp-enable-on-type-formatting nil)
  (lsp-eldoc-render-all nil)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold 4000)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  :config
  ;; Flycheck patch checkers
  (require 'lsp-diagnostics)
  (require 'flycheck)
  (lsp-diagnostics-flycheck-enable)
  ;; Golang
  (defun lsp-go-install-save-hooks ()
    ;; (flycheck-add-next-checker 'lsp '(t . golangci-lint) 'append)

    (flycheck-add-next-checker 'lsp '(warning . go-gofmt) 'append)
    (flycheck-add-next-checker 'lsp '(warning . go-golint))
    (flycheck-add-next-checker 'lsp '(warning . go-errcheck))
    (flycheck-add-next-checker 'lsp '(warning . go-staticcheck))

    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


  ;; Override company backends for lsp
  ;; (setq +lsp-company-backends '(company-tabnine :separate company-capf))
  (setq +lsp-company-backends '(company-tabnine :separate company-yasnippet))

  (setq lsp-disabled-clients '(html html-ls))
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\venv\\'")
  (setq lsp-eldoc-hook nil))

;;;;; LSP-UI

(use-package! lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-diagnostic-max-line-length 200
        lsp-ui-sideline-diagnostic-max-lines 5
        lsp-ui-doc-delay 2
        lsp-ui-doc-position 'top
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-border +m-color-main))



;;;; NGINX

(use-package! company-nginx
  :after nginx-mode
  :config (add-hook 'nginx-mode-hook (lambda () (add-to-list 'company-backends #'company-nginx))))

(use-package! nginx-mode
  :defer 10)

;;;; MUTT-MODE

(use-package! mutt-mode)

;;;; NOTDEFT

(use-package! notdeft
  :config
  (setq notdeft-directories '("~/Nextcloud/Notes/org"
                              "~/Nextlcoud/Notes/org/daily"
                              "~/Nextcloud/Notes/org/TipJar"))
  (add-hook 'notdeft-load-hook 'notdeft-xapian-make-program-when-uncurrent))

;;;; OUTSHINE

(use-package! outshine
  :config
  (map! :after outshine
        :map emacs-lisp-mode-map
        "TAB" #'outshine-cycle)
  (add-hook 'emacs-lisp-mode-hook #'outshine-mode)
  (defvar outline-minor-mode-prefix "\M-#"))

;;;; PAPERLESS

(use-package! paperless
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

;;;; PYTHON MODE

(use-package! python-mode
  :defer 0.5
  :hook (python-mode . format-all-mode)
  :config
  (setq pytnon-indent-level 4)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 4))))

;;;;; LSP PYRIGHT

(use-package! lsp-pyright
  :defer 0.5
  :config
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-log-level "trace"))
;;;; RAINBOW MODE

(use-package rainbow-mode
  :hook (((css-mode scss-mode org-mode emacs-lisp-mode typescript-mode js-mode). rainbow-mode))
  :defer 2.3)

;;;; SALT MODE

(use-package! salt-mode)

;;;; SYSTEMD MODE

(use-package! systemd)

(map! :map systemd-mode
      :localleader
      :prefix ("h" . "help")
      "d" #'systemd-doc-directives
      "o" #'systemd-doc-open)

;;;; TREEMACS

(use-package! treemacs
  :defer 3
  :custom
  (treemacs-width 35)
  :config
  (setq +treemacs-git-mode 'extended))

;;;; TREESITTER

(use-package! tree-sitter-langs
  :defer 6)

(use-package! tree-sitter
  :after tree-sitter-langs
  :hook ((go-mode typescript-mode css-mode html-mode scss-mode ng2-mode js-mode python-mode rust-mode ng2-ts-mode ng2-html-mode) . tree-sitter-hl-mode)
  :config
  (push '(ng2-html-mode . html) tree-sitter-major-mode-language-alist)
  (push '(ng2-ts-mode . typescript) tree-sitter-major-mode-language-alist)
  (push '(scss-mode . css) tree-sitter-major-mode-language-alist))


;;;; UNDO

(use-package! undo-tree
  :defer 0.3
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/tmp/undo")))
  ;; (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode))

;;;; VLF

(use-package! vlf-setup
  :defer-incrementally  vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;;;; WAKATIME

(defun marty/startwakatime ()
  (interactive)
  (setq wakatime-api-key (auth-source-pass-get 'secret "Application/wakatime/apikey"))
  (global-wakatime-mode))

(use-package! wakatime-mode
  :defer 1.2
  :config
  (add-hook 'doom-first-buffer-hook  #'marty/startwakatime)
  (setq wakatime-cli-path "/usr/bin/wakatime"))

;;;; WEB MODE

(use-package! web-mode
  :defer 0.5
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (setq web-mode-comment-formats
        '(("java"       . "/*")
          ("javascript" . "//")
          ("typescript" . "//")
          ("vue"        . "//")
          ("php"        . "/*")
          ("pug"        . "//")
          ("css"        . "/*")))
  ;; (add-to-list 'web-mode-comment-formats '("pug" . "//"))
  ;; (setcdr (assoc "javascript" web-mode-comment-formats) "//")
  ;; (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

;;;;; HTML

(use-package! emmet-mode
  :hook ((scss-mode . emmet-mode) (css-mode . emmet-mode) (ng2-html-mode . emmet-mode) (html-mode . emmet-mode))
  :defer 5)


;;;;; JSON

(use-package! json-mode
  :defer 5
  :hook (json-mode . format-all-mode))


;;; TEMP / FIXUP

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            latex-mode))

;;;; XTERM SET-WINDOW-TITLE

(setq xterm-set-window-title t)
(defadvice! fix-xterm-set-window-title (&optional terminal)
  :before-while #'xterm-set-window-title
  (not (display-graphic-p terminal)))

;;;; MMDECODE

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;;;;; MARGINALIA

(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

;;; CUSTOM

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
