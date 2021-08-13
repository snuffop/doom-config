;; $DOOMDIR/config.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2021, Marty Buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Global
(setq user-full-name "Marty Buchaus")
(setq user-mail-address "marty@dabuke.com")

(setq-default enable-local-variables t)            ; Allow for reading the local variables file
(setq-default delete-by-moving-to-trash t)
(setq-default window-combination-resize t)
(setq-default x-stretch-cursor t)

(setq undo-limit 80000000)                         ; Raise undo-limit to 80Mb
(setq evil-want-fine-undo t)                       ; By default while in insert all changes are one big blob. Be more granular
(setq auto-save-default t)                         ; Nobody likes to loose work, I certainly don't
(setq truncate-string-ellipsis "…")                ; Unicode ellispis are nicer than "...", and also save /precious/ space
(setq password-cache-expiry nil)                   ; I can trust my computers ... can't I?
(setq scroll-margin 2)                             ; It's nice to maintain a little margin
(setq confirm-kill-emacs nil)                      ; Stop hounding me and quit

(setq display-time-24hr-format t)
(display-time-mode 1)                             ; Enable time in the mode-line

(global-subword-mode 1)

;; Remove the s/S from evil snipe
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(setq company-idle-delay 0.5)

;; Fonts

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 15)
      doom-unicode-font (font-spec :family "Symbola" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "Firacode Nerd Font" :size 24))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

;; Faces
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(setq global-prettify-symbols-mode t)

(custom-set-faces!
  '(mode-line :family "DejaVu Sans Mono" :height 100)
  '(mode-line-inactive :family "DejaVu Sans Mono" :height 100))

(add-hook! 'org-mode-hook #'mixed-pitch-mode)

;;;; Theme

(setq doom-theme 'doom-dracula )

;;;; Spelling

(after! spell-fu
  (setq spell-fu-idle-delay 0.5)
  (setq ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir))
  )

;;;; Line Numbers

(setq display-line-numbers-type 'relative)

;;;; Load Org Mode

(setq org-directory "~/Nextcloud/Notes/org/")
(setq org-roam-directory "~/Nextcloud/Notes/org/")
(setq org-contacts-files '("~/Nextcloud/Notes/org/contacts.org"))

;; ORG
(load! "org-mode.el")

;;;;; org-caldav

(use-package! org-caldav
  :after org
  :init
  ;; This is the sync on close function; it also prompts for save after syncing so
  ;; no late changes get lost
  (defun org-caldav-sync-at-close ()
    (org-caldav-sync)
    (save-some-buffers))

  ;; This is the delayed sync function; it waits until emacs has been idle for
  ;; "secs" seconds before syncing.  The delay is important because the caldav-sync
  ;; can take five or ten seconds, which would be painful if it did that right at save.
  ;; This way it just waits until you've been idle for a while to avoid disturbing
  ;; the user.
  (defvar org-caldav-sync-timer nil
    "Timer that `org-caldav-push-timer' used to reschedule itself, or nil.")
  (defun org-caldav-sync-with-delay (secs)
    (when org-caldav-sync-timer
      (cancel-timer org-caldav-sync-timer))
    (setq org-caldav-sync-timer
          (run-with-idle-timer
           (* 1 secs) nil 'org-caldav-sync)))

  (setq org-caldav-calendars
        '((:calendar-id "personal"
           :files ("~/Nextcloud/Notes/org/Calendar.org")
           :inbox "~/Nextcloud/Notes/Calendars/personal-inbox.org"))
        )

  :config (progn
            (setq org-icalendar-alarm-time 1)
            (setq org-caldav-url "https://nextcloud.dabuke.com/remote.php/dav/calendars/marty")
            (setq org-icalendar-timezone "America/New York")
            (setq org-caldav-save-directory (concat user-emacs-directory ".local/cache/"))
            (setq org-caldav-backup-file (concat user-emacs-directory ".local/cache/"))
            (setq org-icalendar-use-deadline t)
            (setq org-icalendar-include-todo t)
            ;; This ensures all org "deadlines" show up, and show up as due dates
            (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
            ;; This ensures "scheduled" org items show up, and show up as start times
            (setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
            ;; Add the delayed save hook with a five minute idle timer
            (add-hook 'after-save-hook
                      (lambda ()
                        (when (eq major-mode 'org-mode)
                          (org-caldav-sync-with-delay 300)))))
  (add-hook 'kill-emacs-hook 'org-caldav-sync-at-close))

;;;; Load Functions.el

(load! "functions.el")

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;;;; Load MU4E.el

(load! "mu4e.el")

(use-package! mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))

;;;; Leader keys and keybindings

(setq doom-localleader-key ",")

(load! "keybindings.el")

;;;;; aggressive indent

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

;;;;; autoinsert

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
            (define-auto-insert "/[0-9]\\{8\\}.org$" ["defaultJournal.org" marty/autoinsert-yas-expand])))

;;;;; i3 Window manager config

(use-package! i3wm-config-mode
  :defer t)

;;;;; Khard

(use-package! khardel
  :defer t)

;;;; Magit

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

;;;;; Outshine

(use-package! outshine
  :defer t)

(after! outshine
  (map! :after outshine
        :map emacs-lisp-mode-map
        "TAB" #'outshine-cycle)
  (add-hook 'emacs-lisp-mode-hook #'outshine-mode)
  (defvar outline-minor-mode-prefix "\M-#"))

;;;;; Paperless

(use-package paperless
  :init (require 'org-paperless)
  :config (progn
            (custom-set-variables
             '(paperless-capture-directory "~/Nextcloud/Documents/INBOX/")
             '(paperless-root-directory "~/Nextcloud/Documents"))))

(after! paperless
  (map! :leader
        :prefix "a"
        "X"  #'paperless)
  (map! :after paperless
        :localleader
        :mode paperless-mode
        "d"  #'paperless-display
        "r"  #'paperless-rename
        "R"  #'paperless-scan-directories
        "f"  #'paperless-file
        "X"  #'paperless-execute))

;;;;; Salt Mode

(use-package! salt-mode
  :defer t
  :config
  (add-hook 'salt-mode-hook
            (lambda ()
              (flyspell-mode 1))))

;;;;; Systemd Mode

(use-package! systemd
  :defer t)

(map! :map systemd-mode
      :localleader
      :prefix ("h" . "Help")
      "d" #'systemd-doc-directives
      "o" #'systemd-doc-open)

;;;;; Counsel Tramp

(use-package! counsel-tramp
  :after 'tramp )

(after! counsel-tramp

  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  (defvar disable-tramp-backups '(all))
  (setenv "SHELL" "/bin/bash")

  (setq tramp-default-method "scp")
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-completion-reread-directory-timeout nil)
  (setq helm-tramp-control-master t)

  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  (add-hook 'counsel-tramp-pre-command-hook
            #'(lambda () (global-aggressive-indent-mode 0)
               (projectile-mode 0)
               (editorconfig-mode 0)))

  (add-hook 'counsel-tramp-quit-hook
            #'(lambda () (global-aggressive-indent-mode 1)
               (projectile-mode 1)
               (editorconfig-mode 1)))

;;;;;; List of Hosts

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
          ;; RS
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
          )))
;; End of  List of hosts

;;;; Treemacs

(setq +treemacs-git-mode 'extended)

;;;;; VLF

(use-package! vlf-setup
  :defer-incrementally  vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;;;;; Wakatime

(use-package! wakatime-mode
  :ensure t
  :hook (doom-first-buffer . 'global-wakatime-mode)
  :config
  (setq wakatime-cli-path "/usr/bin/wakatime")
  (setq wakatime-api-key (auth-source-pass-get 'secret "Application/wakatime/apikey")))

;;;; Custom

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
