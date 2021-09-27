;;; config.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2021, Marty Buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; CODE
;;;; GLOBAL

(setq user-full-name "Marty Buchaus")
(setq user-mail-address "marty@dabuke.com")

(setq-default enable-local-variables t)            ; allow for reading the local variables file
(setq-default window-combination-resize t)
(setq-default x-stretch-cursor t)

(setq undo-limit 80000000)                         ; raise undo-limit to 80mb
(setq evil-want-fine-undo t)                       ; by default while in insert all changes are one big blob. be more granular
(setq auto-save-default t)                         ; nobody likes to loose work, i certainly don't
(setq truncate-string-ellipsis "…")                ; unicode ellispis are nicer than "...", and also save /precious/ space
(setq password-cache-expiry nil)                   ; i can trust my computers ... can't i?
(setq scroll-margin 2)                             ; it's nice to maintain a little margin
(setq confirm-kill-emacs nil)                      ; stop hounding me and quit

(setq display-time-24hr-format t)
(display-time-mode 1)                             ; enable time in the mode-line

(global-subword-mode 1)

(after! projectile
  (setq projectile-project-search-path '("~/source")))
;;  (setq projectile-project-root-files-bottom-up (remove ".git" projectile-project-root-files-bottom-up)))

;;;;; COMPANY

(setq company-idle-delay 0.5)

;;;;; EVIL-SETTINGS

(setq! evil-want-Y-yank-to-eol nil)

;;;; UI
;;;;; FONTS

(setq doom-font (font-spec :family "firacode nerd font mono" :size 15)
      doom-unicode-font (font-spec :family "symbola" :size 15)
      doom-variable-pitch-font (font-spec :family "ubuntu" :size 15)
      doom-big-font (font-spec :family "firacode nerd font mono" :size 24))

;;;;; FACES
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(setq global-prettify-symbols-mode t)

(custom-set-faces!
  '(mode-line :family "firacode nerd font mono" :height 100)
  '(mode-line-inactive :family "firacode nerd font mono" :height 100))

(add-hook! 'org-mode-hook #'mixed-pitch-mode)

;;;;; THEME
(setq doom-theme 'doom-dracula )

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(after! doom-modeline
  (setq all-the-icons-scale-factor 1.1
        auto-revert-check-vc-info t
        doom-modeline-major-mode-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon (display-graphic-p)
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-vcs-max-length 60)
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals persp-name buffer-info matches remote-host github debug)
    '(vcs github mu4e grip gnus checker misc-info repl lsp " ")))

;;;;; LINE NUMBERS

(setq display-line-numbers-type 'relative)

;; remove numbers from these modes
;;
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;;;; REGISTERS

(defun marty/set-patching-macro-registers ()
  "evil keyboard macros for patching,  running docker containers"
  (interactive)
  (evil-set-register ?e [?0 ?i ?* ?* ?* ?* ?* ?* ?  escape ?0])
  (evil-set-register ?b [?0 ?o escape ?0 ?i ?# ?+ ?e ?n ?d ?_ ?e ?x ?a ?m ?p ?l ?e escape ?0] )
  (evil-set-register ?t [?0 ?o ?i backspace ?# ?+ ?b ?e ?g ?i ?n ?_ ?e ?x ?a ?m ?p ?l ?e escape ?0]))

;;;; SPELLING

(after! flyspell
        (setq flyspell-lazy-idle-seconds 2)
        (setq ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir)))

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
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook    #'aggressive-indent-mode)
  (add-hook 'php-mode-hook #'aggressive-indent-mode)
  (add-hook 'hy-mode-hook #'aggressive-indent-mode))

(global-aggressive-indent-mode 1)

(use-package alert
  :defer t)

(use-package! all-the-icons-completion)
(all-the-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

;;;; AUTOINSERT

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
            (define-auto-insert "nsi-documentation/[^/]+\\.org?$" ["NSI-Documentation.org" marty/autoinsert-yas-expand])
            (define-auto-insert "nsi-documentation/.+/[^/]+\\.org?$" ["NSI-Documentation.org" marty/autoinsert-yas-expand])
            (define-auto-insert "nsi-documentation/tipjar/[^/]+\\.org?$" ["NSI-Documentation-tipjar.org" marty/autoinsert-yas-expand])
            (define-auto-insert "nsi-documentation/tva/[^/]+\\.org?$" ["NSI-Documentation-TVA.org" marty/autoinsert-yas-expand])
            (define-auto-insert "nsi-documentation/tva/scanreports/.+[^/]+\\.org?$" ["NSI-Documentation-TVA-scanreport.org" marty/autoinsert-yas-expand])
            (define-auto-insert "nsi-documentation/patching/.+[^/]+\\.org?$" ["nsi-documentation-patching-notes.org" marty/autoinsert-yas-expand])
            (define-auto-insert "masons/[^/].+\\.org?$" ["masonsMeetingMinuets.org" marty/autoinsert-yas-expand])
            (define-auto-insert "daily/[^/].+\\.org?$" ["defaultRoamDaily.org" marty/autoinsert-yas-expand])
            (define-auto-insert "/[0-9]\\{8\\}.org$" ["defaultJournal.org" marty/autoinsert-yas-expand])))

;;;;; FUNCTIONS

(defun marty/autoinsert-yas-expand ()
  "This is used by the autoinsert package to grab and expand
templates into newly created files"
  (let ((template ( buffer-string )))
    (delete-region (point-min) (point-max))
    (yas-expand-snippet template)
    (evil-insert-state)))

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
         "i"  #'browse-kill-ring-append-insert))

  )

;;;; DASHBOARD

(use-package dashboard
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

;;;; ELPHER

(use-package! elpher)

;;;; I3 WINDOW MANAGER CONFIG

(use-package! i3wm-config-mode
  :defer t)

;;;; KHARD

(use-package! khardel
  :defer t)

(setq ledger-post-amount-alignment-column 69)

;;;; MAGIT

(setq magit-revision-show-gravatars '("^author:     " . "^commit:     "))

;;;; OUTSHINE

(use-package! outshine
  :config
  (map! :after outshine
        :map emacs-lisp-mode-map
        "TAB" #'outshine-cycle)
  (add-hook 'emacs-lisp-mode-hook #'outshine-mode)
  (defvar outline-minor-mode-prefix "\M-#"))

;;;; PAPERLESS

(use-package paperless
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

;;;; SALT MODE

(use-package! salt-mode
  :defer t)

;;;; SYSTEMD MODE

(use-package! systemd
  :defer t)

(map! :map systemd-mode
      :localleader
      :prefix ("h" . "help")
      "d" #'systemd-doc-directives
      "o" #'systemd-doc-open)

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

;;;; TREEMACS

(setq +treemacs-git-mode 'extended)

;;;; VLF

(use-package! vlf-setup
  :defer-incrementally  vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;;;; WAKATIME
(defun marty/startwakatime ()
  (interactive)
  (setq wakatime-api-key (auth-source-pass-get 'secret "Application/wakatime/apikey"))
  (global-wakatime-mode))

(use-package! wakatime-mode
  :config
  (add-hook 'doom-first-buffer-hook  #'marty/startwakatime)
  (setq wakatime-cli-path "/usr/bin/wakatime"))

;;; TEMP / FIXUP
;;;; XTERM SET-WINDOW-TITLE

(setq xterm-set-window-title t)
(defadvice! fix-xterm-set-window-title (&optional terminal)
  :before-while #'xterm-set-window-title
  (not (display-graphic-p terminal)))

;;;; MMDECODE

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;;; CUSTOM

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
