;;; $doomdir/keybindings.el --- summary -*- lexical-binding: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2021, marty buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; code:

(setq doom-localleader-key ",")

;;;; global keybindings

(define-key! help-map
  "h"    #'helpful-at-point)

(map!
 ;;:n "c-:"    #'+spell/correct
 :n "c-;"    #'embark-act
 :n "c-,"    #'+spell/next-error)

;;;; leader keybindings

(map! :leader
      "tab"  #'evil-switch-to-windows-last-buffer
      "spc"  #'execute-extended-command
      ;;; <leader> a --- application
      (:prefix-map ("a" . "application")
       "m"  #'=mu4e
       ;; org
       (:prefix-map ("o" . "org")
        "/" #'org-occur-in-agenda-files
        "a" #'org-agenda-list
        "t" #'org-todo-list
        "l" #'org-store-link
        "m" #'org-tags-view
        "o" #'org-agenda
        "s" #'org-search-view
        "t" #'org-todo-list
        (:prefix-map ("c" . "clock/contacts")
         "c"  #'org-clock-cancel
         "g"  #'org-clock-goto
         "i"  #'org-clock-in
         "j"  #'org-clock-jump-to-current-clock
         "o"  #'org-clock-out
         "r"  #'org-resolve-clocks
         "i"  #'org-clock-in-last
         (:prefix-map ("t" . "org-timer")
          "t"  #'org-timer-set-timer
          "p"  #'org-timer-pause-or-continue
          "q"  #'org-timer-stop)))
       ;; tools
       (:when (featurep! :tools pass)
        (:prefix-map ("p" . "pass")
         "/"  #'+pass/consult
         "c"  #'password-store-edit
         "d"  #'password-store-remove
         "g"  #'password-store-generate
         "i"  #'password-store-insert
         "r"  #'password-store-rename
         "w"  #'password-store-url
         "y"  #'password-store-copy
         "d"  #'password-store-clear
         "i"  #'password-store-init
         (:prefix-map ("o" . "otp")
          "a" #'password-store-otp-append
          "i" #'password-store-otp-insert
          "y" #'password-store-otp-token-copy
          "a" #'password-store-otp-append-from-image
          "y" #'password-store-otp-uri-copy)))
       (:prefix-map ("t" . "tramp")
        "c"  #'tramp-cleanup-all-connections
        "b"  #'tramp-cleanup-all-buffers
        "c"  #'tramp-cleanup-this-connection
        "t"  #'counsel-tramp
        "q"  #'counsel-tramp-quit))
;;;;; <leader> l --- workspace / layout
      (:when (featurep! :ui workspaces)
       (:prefix-map ("l" . "workspace")
        :desc "display tab bar"           "spc" #'+workspace/display
        :desc "cycle tab bar"             "tab" #'+workspace/cycle
        :desc "switch workspace"          "."   #'+workspace/switch-to
        :desc "switch to last workspace"  "`"   #'+workspace/other
        :desc "move workspace left"       "<"   #'+workspace/swap-left
        :desc "move workspace right"      ">"   #'+workspace/swap-right
        :desc "new workspace"             "n"   #'+workspace/new
        :desc "create named workspace"    "n"   #'+workspace/new-named
        :desc "open workspace from file"  "o"   #'+workspace/load
        :desc "save workspace to file"    "s"   #'+workspace/save
        :desc "delete session"            "x"   #'+workspace/kill-session
        :desc "delete this workspace"     "d"   #'+workspace/delete
        :desc "rename workspace"          "r"   #'+workspace/rename
        :desc "switch workspace"          "l"   #'+workspace/switch-to
        :desc "restore last session"      "r"   #'+workspace/restore-last-session
        :desc "next workspace"            "]"   #'+workspace/switch-right
        :desc "previous workspace"        "["   #'+workspace/switch-left
        :desc "switch to 1st workspace"   "1"   #'+workspace/switch-to-0
        :desc "switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
        :desc "switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
        :desc "switch to 4th workspace"   "4"   #'+workspace/switch-to-3
        :desc "switch to 5th workspace"   "5"   #'+workspace/switch-to-4
        :desc "switch to 6th workspace"   "6"   #'+workspace/switch-to-5
        :desc "switch to 7th workspace"   "7"   #'+workspace/switch-to-6
        :desc "switch to 8th workspace"   "8"   #'+workspace/switch-to-7
        :desc "switch to 9th workspace"   "9"   #'+workspace/switch-to-8
        :desc "switch to final workspace" "0"   #'+workspace/switch-to-final)))

;;;;; <leader n --- notes

(map! :leader
      :prefix "n"
      "b" #'marty/org-roam-capture-inbox)

;;;;; <leader> o --- open

(map! :leader
      :prefix "o"
      (:prefix-map ("m" . "my")
       :desc "0mobile"       "0" #'(lambda () (interactive) (find-file (concat org-directory "0mobile.org")))
       :desc "desktop"       "d" #'(lambda () (interactive) (find-file (concat org-directory "desktop.org")))
       :desc "contacts"      "o" #'(lambda () (interactive) (find-file (concat org-directory "contacts.org")))
       :desc "tasks"         "g" #'(lambda () (interactive) (find-file (concat org-directory "tasks.org")))
       :desc "habits"        "h" #'(lambda () (interactive) (find-file (concat org-directory "habits.org")))
       :desc "read later"    "l" #'(lambda () (interactive) (find-file (concat org-directory "read-later.org")))
       :desc "someday"       "s" #'(lambda () (interactive) (find-file (concat org-directory "someday.org")))
       :desc "tip jar"       "t" #'(lambda () (interactive) (find-file (concat org-directory "tipjar.org")))
       (:prefix-map ("c" . "+config")
        :desc "keybindings"  "k"  #'(lambda () (interactive) (find-file (concat doom-private-dir "keybindings.el")))
        :desc "config"       "c"  #'(lambda () (interactive) (find-file (concat doom-private-dir "config.el")))
        :desc "org"          "o"  #'(lambda () (interactive) (find-file (concat doom-private-dir "org-mode.el")))
        :desc "init"         "i"  #'(lambda () (interactive) (find-file (concat doom-private-dir "init.el")))
        :desc "packages"     "p"  #'(lambda () (interactive) (find-file (concat doom-private-dir "packages.el")))
        :desc "mu4e"         "m"  #'(lambda () (interactive) (find-file (concat doom-private-dir "mu4e.el"))))
       (:prefix-map ("c" . "calendar")
        :desc "calendar"      "c"  #'(lambda () (interactive) (find-file (concat org-directory "calendar.org")))
        :desc "vdirsync"      "s"  #'khalel-run-vdirsyncer
        :desc "import"        "i"  #'khalel-import-upcoming-events)))

;;;; mode maps
;;;;; override org mode map

(map! :after org
      :map org-mode-map
      :localleader
      :prefix "m"
      "a"  #'marty/org-roam-move-todo-to-today
      "b"  #'marty/org-roam-capture-inbox
      "i"  #'org-roam-node-insert-immediate
      "j"  #'org-roam-dailies-capture-today
      "p"  #'marty/org-roam-find-project
      "s"  #'org-roam-db-sync
      "s"  #'marty/org-roam-rg-search
      :prefix "md"
      "p"  #'org-roam-dailies-goto-previous-note
      "n"  #'org-roam-dailies-goto-next-note
      )

;;;; dired keybindings
;;;;;  ts file trigger keybinding
(define-key! dired-mode-map
  (kbd "c-t") #'marty/dired-copy-filename-as-tsfile-link)

(map! :leader
      (:prefix ("d" . "dired")
       :desc "open dired" "d"                   #'dired
       :desc "dired jump to current" "j"        #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "peep-dired image previews" "d p" #'peep-dired
        :desc "create tsfile link" "d t"        #'marty/dired-copy-filename-as-tsfile-link
        :desc "dired view file" "d v"           #'dired-view-file)))
;; make 'h' and 'l' go back and forward in dired. much faster to navigate the directory structure!

(evil-define-key 'normal dired-mode-map
  (kbd "m-ret") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-view-file
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "c") 'dired-do-copy
  (kbd "d") 'dired-do-delete
  (kbd "j") 'dired-goto-file
  (kbd "m") 'dired-chmod
  (kbd "o") 'dired-chown
  (kbd "p") 'dired-do-print
  (kbd "r") 'dired-rename
  (kbd "t") 'dired-do-touch
  (kbd "y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; if peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
