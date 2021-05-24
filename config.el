;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Marty Buchaus"
      user-mail-address "marty@dabuke.com")

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 10.5 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 12))

(setq doom-theme 'doom-dracula)

(setq display-line-numbers-type 'relative)

;;;; Org Mode

(setq org-directory "~/Nextcloud/Notes/org")

(use-package! org
  :config
  (setq org-attach-set-directory "~/Nextcloud/Notes/org/data")
  (setq org-contacts-files '("~/Nextcloud/Notes/org/contacts.org"))
  (setq org-default-notes-file (concat org-directory "0mobile.org"))
  (setq org-download-image-dir "~/Nextcloud/Notes/images/")
  (setq org-id-locations-file "~/Nextcloud/Notes/org-id-locations")
  (setq org-projectile-file "todo.org")
  )

;;;;; Org Roam
(use-package! org-roam
  :config

  (setq org-roam-directory "~/Nextcloud/Notes/org")
  (setq org-roam-dailies-directory "~/Nextcloud/Notes/org/daily")
  (setq org-roam-buffer-width 0.15)
  (setq org-roam-index-file "~/Nextcloud/Notes/org/index.org")
  (setq org-roam-link-title-format "R:%s")

  (custom-set-faces '(org-roam-link ((t (:inherit org-link :foreground "#F2C3BD"))))))
;;;;;; Org Roam Binding

  (map! :after org
        :map org-mode-map
        :localleader
        :prefix ("m" . "org-roam")
        "b" #'org-roam-switch-to-buffer
        "f" #'org-roam-find-file
        "g" #'org-roam-graph
        "i" #'org-roam-insert
        "I" #'org-roam-insert-immediate
        "m" #'org-roam
        "t" #'org-roam-tag-add
        "T" #'org-roam-tag-delete
        (:prefix ("d" . "by date")
         :desc "Find previous note" "b" #'org-roam-dailies-find-previous-note
         :desc "Find date"          "d" #'org-roam-dailies-find-date
         :desc "Find next note"     "f" #'org-roam-dailies-find-next-note
         :desc "Find tomorrow"      "m" #'org-roam-dailies-find-tomorrow
         :desc "Capture today"      "n" #'org-roam-dailies-capture-today
         :desc "Find today"         "t" #'org-roam-dailies-find-today
         :desc "Capture Date"       "v" #'org-roam-dailies-capture-date
         :desc "Find yesterday"     "y" #'org-roam-dailies-find-yesterday
         :desc "Find directory"     "." #'org-roam-dailies-find-directory))
;;;;;; Org Roam Capture Templates

(after! org-roam
  (setq  org-roam-dailies-capture-templates
         '(
           ("d" "default" plain (function org-roam--capture-get-point)
            "%?"
            :file-name "daily/%<%Y-%m-%d>"
            ;; :head "#+TITLE: Daily Notes for %<%A, %B %d %Y>"
            :unnarrowed t)
           ("t" "Tasks" entry
            #'org-roam-capture--get-point
            "* TODO  %?"
            :file-name "daily/%<%Y-%m-%d>"
            :olp ("Tasks"))
           ("r" "Rackspace" entry
            #'org-roam-capture--get-point
            "* %<%H:%M>  %?"
            :file-name "daily/%<%Y-%m-%d>"
            :olp ("Rackspace"))
           ("j" "Journal" entry
            #'org-roam-capture--get-point
            "* %<%H:%M>  %?"
            :file-name "daily/%<%Y-%m-%d>"
            :olp ("Journal")))))

;;;;; Org Capture Templates

(after! org
  (setq org-capture-templates
        '(("t" "Task" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           (file "~/.config/doom/templates/todo.orgcaptmpl"))
          ("c" "Contacts" entry (file "~/Nextcloud/Notes/org/contacts.org")
           (file "~/.config/doom/templates/contact.orgcaptmpl"))
          ("p" "Protocol" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox"entry)
           (file "~/.config/doom/templates/org-templates/protocol-entry.orgcaptmpl"))
          ("R" "Remember-mutt" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Mail")
           (file "~/.config/doom/templates/org-templates/mail.orgcaptmpl"))
          ("L" "Protocol Link" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           (file "~/.config/doom/templates/org-templates/protocol-link.orgcaptmpl"))
          ("w" "Web site" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Inbox")
           (file "~/.config/doom/templates/org-templates/weblink.orgcaptmpl"))
          ("s" "Simple" entry
           (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Popup")
           "%[~/.emacs.d/.org-popup]" :immediate-finish t :prepend t)

          ("m" "Email Workflow")
          ("mf" "Follow Up" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\nSCHEDULED:%t\n%a\n\n%i")
          ("ma" "auto Follow Up" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\n%a\n\n%i" :immediate-finish t)
          ("mF" "Follow Up With Deadline" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE:%(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n%a\n\n%i")
          ("mr" "Read Later" entry (file+olp "~/Nextcloud/Notes/org/0mobile.org" "Read Later")
          "* TODO Read  Later on %:subject\nSCHEDULED:%t\n%a\n\n%i":immediate-finish t)
          ("mm" "Masons Follow Up" entry (file+olp "~/Nextcloud/Notes/org/Masons.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject %a\nSCHEDULED:%t\n\\n%i")
          ("mR" "Workflow Rackspace")
          ("mRf" "Follow Up" entry (file+olp "~/Nextcloud/Notes/org/Rackspace.org" "Follow Up")
           "* TODO Follow up with %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE:%(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n%a\n\n%i")
          ("mRr" "Read Later" entry (file+olp "~/Nextcloud/Notes/org/Rackspace.org" "Read Later")
           "* TODO Read  Later with %:fromname on %:subject\nSCHEDULED:%t\n%a\n\n%i" :immediate-finish t)
          ))
  (setq org-protocol-default-template-key "t"))

;;;;; Org Agenda
(after! org

  (setq org-agenda-compact-blocks t)
  (setq org-agenda-include-deadlines t)
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-window-setup 'current-window)
  (setq org-deadline-warning-days 14)

  ;; Ignore scheduled tasks in task list
  (setq org-agenda-todo-ignore-scheduled 'all)
  (setq org-agenda-todo-ignore-deadlines 'far)
  ;; Skip Finished Items
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-files '("~/Nextcloud/Notes/org"
                           "~/Nextcloud/Notes/org/daily"
                           "~/.cache/calendar/google.org"
                           "~/.cache/calendar/rackspace.org"
                           "~/.cache/calendar/tatjana.org")))



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

;;;;;
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
;;;;; Mutt Mail Config

;; Used to Call modes and edit email for use with Neomutt

(add-to-list 'auto-mode-alist '("/tmp/neomutt.*"        . message-mode))
(add-to-list 'auto-mode-alist '("/tmp/mutt.*"           . message-mode))
(add-to-list 'auto-mode-alist '("\\.mutt\\'"            . mutt-mode))

;;  THE CAUSE OF THE ATTACHMENT ISSUES
;; (setq mail-header-separator "")

(add-hook 'message-mode-hook 'auto-fill-mode)
;; (add-hook 'message-mode-hook 'flyspell-mode)

(defvar message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c")  '(lambda ()
                                        "save and exit quickly"
                                        (interactive)
                                        (save-buffer)
                                        (server-edit)))
    map)
  "Add Mutt C-c C-c to save and exit frame"
  )

;; Function to open message with terminal and open the message with neomutt based on id

(defun mutt-open-message (message-id)
  "In neomutt, open the email with the the given Message-ID"
  (let*
      ((message-id
        (replace-regexp-in-string "^/*" "" message-id))
       (mail-file
        (replace-regexp-in-string
         "\n$" "" (shell-command-to-string
                   (format "notmuch search --output=files id:%s" message-id))))
       (mail-box (replace-regexp-in-string "/home/marty/Mail" "" mail-file))
       (mail-dir (replace-regexp-in-string "/\\(cur\\|new\\|tmp\\)/$" ""
                                           (file-name-directory mail-box)))
       (mutt-keystrokes
        (format "macro index - l~i%s; push -\\nb\\n" (shell-quote-argument message-id)))
        (mutt-command (format "neomutt -f '=%s' -e '%s'" mail-dir  mutt-keystrokes)))

    (message "Launching neomutt for message %s" message-id)
    ;;(message " %s" mutt-command)
    (call-process "setsid" nil nil nil
                  "-f" "termite" "-e"
                  mutt-command)))

(after! org
  (org-add-link-type "message" 'mutt-open-message))

;; end of mutt
