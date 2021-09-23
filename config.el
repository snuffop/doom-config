;; Global
(setq user-full-name "Marty Buchaus")
(setq user-mail-address "marty@dabuke.com")

(setq-default enable-local-variables t)            ; Allow for reading the local variables file
;; (setq-default delete-by-moving-to-trash t)
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

;; (after! projectile
;;   (setq projectile-project-search-path '("~/Source"))
;;   (setq projectile-project-root-files-bottom-up (remove ".git" projectile-project-root-files-bottom-up)))

;;; World clock
  (setq zoneinfo-style-world-list
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Chicago" "Chicago")
          ("America/New_York" "New York")
          ("Europe/Lisbon" "Lisbon")
          ("Europe/Brussels" "Brussels")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Tokyo" "Tokyo")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z  %A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60)

(setq company-idle-delay 0.5)

;; Fonts

(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 15)
      doom-unicode-font (font-spec :family "Symbola" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :size 24))

;; Faces
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(setq global-prettify-symbols-mode t)

(custom-set-faces!
  '(mode-line :family "FiraCode Nerd Font Mono" :height 100)
  '(mode-line-inactive :family "FiraCode Nerd Font Mono" :height 100))

(add-hook! 'org-mode-hook #'mixed-pitch-mode)

;;;; Theme

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

;;;; Spelling

(after! flyspell
  (setq flyspell-lazy-idle-seconds 2)
  (setq ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir)))

;;;; Line Numbers

(setq display-line-numbers-type 'relative)

;; remove numbers from these modes
;;
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun marty/set-patching-macro-registers ()
 "Evil keyboard macros for patching,  running docker containers"
  (interactive)
  (evil-set-register ?e [?0 ?i ?* ?* ?* ?* ?* ?* ?  escape ?0])
  (evil-set-register ?b [?0 ?o escape ?0 ?i ?# ?+ ?e ?n ?d ?_ ?e ?x ?a ?m ?p ?l ?e escape ?0] )
  (evil-set-register ?t [?0 ?O ?i backspace ?# ?+ ?b ?e ?g ?i ?n ?_ ?e ?x ?a ?m ?p ?l ?e escape ?0]))

;;;; Load Org Mode

(setq org-directory "~/Nextcloud/Notes/org/")
(setq org-roam-directory "~/Nextcloud/Notes/org/")
(setq org-contacts-files '("~/Nextcloud/Notes/org/contacts.org"))

(defun marty/add-other-auto-props-to-org-roam-properties ()
  ;; if the file already exists, don't do anything, otherwise...
  (unless (file-exists-p (buffer-file-name))
    ;; if there's also a CREATION_TIME property, don't modify it
    (unless (org-find-property "CREATION_TIME")
      ;; otherwise, add a Unix epoch timestamp for CREATION_TIME prop
      ;; (this is what "%s" does - see http://doc.endlessparentheses.com/Fun/format-time-string )
      (org-roam-add-property
       (format-time-string "%s"
                           (nth 5
                                (file-attributes (buffer-file-name))))
       "CREATION_TIME"))
    (unless (org-find-property "ORG_CREATION_TIME")
      (org-roam-add-property
       (format-time-string "[%Y-%m-%d %a %H:%M:%S]"
                           (nth 5
                                (file-attributes (buffer-file-name))))
       "ORG_CREATION_TIME"))
    ;; similarly for AUTHOR and MAIL properties
    (unless (org-find-property "AUTHOR")
      (org-roam-add-property user-full-name "AUTHOR"))
    (unless (org-find-property "MAIL")
      (org-roam-add-property user-mail-address "MAIL"))
    ;; also add the latitude and longitude
    (unless (org-find-property "LAT_LONG")
      ;; recheck location:
      (marty/get-lat-long-from-ipinfo)
      (org-roam-add-property (concat (number-to-string calendar-latitude) "," (number-to-string calendar-longitude)) "LAT-LONG"))))

;; hook to be run whenever an org-roam capture completes
(add-hook 'org-roam-capture-new-node-hook #'marty/add-other-auto-props-to-org-roam-properties)

;; function to find latitude & longitude
;;                      (requires curl to be installed on system)
(setq calendar-latitude 0)
(setq calendar-longitude 0)
(defun marty/get-lat-long-from-ipinfo ()
  (let*
      ((latlong (substring
                 (shell-command-to-string "curl -s 'https://ipinfo.io/loc'")
                   0 -1))
       (latlong-list (split-string latlong ",")))
    (setq calendar-latitude (string-to-number (car latlong-list)))
    (setq calendar-longitude (string-to-number (cadr latlong-list)))))

;; ORG
(load! "org-mode.el")

(when (string= (system-name) "archovo.home.snuffy.org")
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
              (setq org-caldav-debug-level 0)
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
    ;; (add-hook 'kill-emacs-hook 'org-caldav-sync-at-close)
    ))

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

(use-package! mu4e-marker-icons
  :after mu4e
  :init (mu4e-marker-icons-mode 1))

;;;; Leader keys and keybindings

(setq doom-localleader-key ",")

(load! "keybindings.el")

(defun marty/startActivityWatchMode ()
  (interactive)
  (global-activity-watch-mode))

(use-package! activity-watch-mode
  :config
  (add-hook 'doom-first-buffer-hook #'marty/startActivityWatchMode))

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

(use-package! browse-kill-ring
  :config
  (progn
    (defadvice yank-pop (around kill-ring-browse-maybe (arg))
      "If last action was not a yank, run `browse-kill-ring' instead."
      (interactive "p")
      (if (not (eq last-command 'yank))
          (browse-kill-ring)
        (barf-if-buffer-read-only)
        ad-do-it))
    (ad-activate 'yank-pop))

  (map! :leader
      ;;; <leader> k --- Application
        (:prefix-map ("k" . "Kill Ring")
         "k"  #'browse-kill-ring
         "i"  #'browse-kill-ring-append-insert))

  )

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
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

;; Dired Settings
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(use-package! elpher)

;;;;; i3 Window manager config

(use-package! i3wm-config-mode
  :defer t)

(use-package! info-colors
  :after info
  :commands (info-colors-fontify-node)
  :hook (Info-selection . info-colors-fontify-node))

;;;;; Khard

(use-package! khardel
  :defer t)

(setq ledger-post-amount-alignment-column 69)

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

(use-package! org-onenote
  :defer t
  :config
  (setq org-onenote-section-map '(("Marty @ Work"))))

(use-package! org-pretty-table
  :after org
  :hook (org-mode . org-pretty-table-mode))

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
  :defer t)

;;;;; Systemd Mode

(use-package! systemd
  :defer t)

(map! :map systemd-mode
      :localleader
      :prefix ("h" . "Help")
      "d" #'systemd-doc-directives
      "o" #'systemd-doc-open)

(use-package! telega
  :commands (telega))

;;;;; Counsel Tramp

(use-package! counsel-tramp
  :after tramp
  :config (progn
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  (defvar disable-tramp-backups '(all))
  (setenv "SHELL" "/bin/bash")

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

;;;;;; List of Hosts
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
          ))))
;; End of  List of hosts

;;;; Treemacs

(setq +treemacs-git-mode 'extended)

;;;;; VLF

(use-package! vlf-setup
  :defer-incrementally  vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;;;;; Wakatime
(defun marty/startWakatime ()
  (interactive)
  (global-wakatime-mode)
  (setq wakatime-api-key (auth-source-pass-get 'secret "Application/wakatime/apikey")))

(use-package! wakatime-mode
  :ensure t
  :config
  (add-hook 'doom-first-buffer-hook  #'marty/startWakatime)
  (setq wakatime-cli-path "/usr/bin/wakatime"))

;;;; Custom

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
