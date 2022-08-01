;;; $doomdir/config.el --- My Emacs Config File -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; author: Marty Buchaus <marty@dabuke.com>
;; copyright © 2022, Marty Buchaus, all rights reserved.
;; created:  1 November 2019
;;
;;; Commentary
;;
;; See README.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; CODE
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

(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)  ; Make the scratch buffer start in lisp mode

(display-time-mode 1)                              ; enable time in the mode-line
(setq display-time-load-average nil)

(global-subword-mode 1)                            ; CamelCase and it makes refactoring slightly Essie

(set-window-buffer nil (current-buffer))

(add-to-list 'load-path "~/.config/doom/elisp")

;;;;; DAEMON

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
  (add-hook! 'server-after-make-frame-hook
    (unless (string-match-p "\\*draft\\|\\*stdin\\|emacs-everywhere" (buffer-name))
      (switch-to-buffer +doom-dashboard-name))))

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
      (IS-LINUX (setq doom-font (font-spec :family "DejaVuSansMono Nerd Font Mono" :size 14 )
                      doom-big-font (font-spec :family "DejaVuSansMono Nerd Font Mono" :size 22)
                      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15 )
                      doom-unicode-font (font-spec :family "symbola" :size 15)
                      doom-serif-font (font-spec :family "Ubuntu" :size 15))))

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

(remove-hook 'window-setup-hook #'doom-init-theme-h)
(add-hook 'after-init-hook #'doom-init-theme-h 'append)
(delq! t custom-theme-load-path)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(after! doom-themes
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
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

  (set-face-attribute 'mode-line nil :family "Ubuntu" :height 100)
  (set-face-attribute 'mode-line-inactive nil :family "Ubuntu" :height 100)

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

(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(custom-theme-set-faces! 'doom-one
  '(doom-dashboard-banner :foreground "red" :background "#000000" :weight bold)
  '(doom-dashboard-footer :inherit font-lock-constant-face)
  '(doom-dashboard-footer-icon :inherit all-the-icons-red)
  '(doom-dashboard-loaded :inherit font-lock-warning-face)
  '(doom-dashboard-menu-desc :inherit font-lock-string-face)
  '(doom-dashboard-menu-title :inherit font-lock-function-name-face))

(setq fancy-splash-image (expand-file-name "banners/smaller-cute-demon.png" doom-private-dir))


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
 (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))

;;;;; WINDOWS

(setq evil-vsplit-window-right t
      evil-split-window-below t)

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

;;;;;; Embark
;; Open files in (Option) after C-; in embark
;;
(defun my/vsplit-file-open (file)
  (let ((evil-split-window-below t))
  (+evil/window-vsplit-and-follow)
  (find-file file)))

(defun my/split-file-open (file)
  (let ((evil-split-window-below t))
  (+evil/window-split-and-follow)
  (find-file file)))

(map! :after embark
      :map embark-file-map
      "V" 'my/vsplit-file-open
      "X" 'my/split-file-open)

;;;;;; CONSULT

(after! consult
  (evil-collection-init 'consult)
  (set-face-attribute 'consult-file nil :inherit 'consult-buffer)
  (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) ";"))

;;;;;; CONSULT-RECOLL

  (defun my-recoll-format (title url mime-type)
    ;; remove from url the common prefixes /home/jao/{org/doc,doc,...}
    (let* ((u (replace-regexp-in-string "/home/marty/" "" url))
           (u (replace-regexp-in-string
               "\\(doc\\|org/doc\\|.emacs.d/gnus/Mail\\|var/mail\\)/" "" u)))
      (format "%s (%s, %s)"
              (propertize title 'face 'consult-recoll-title-face)
              (propertize u 'face 'consult-recoll-url-face)
              (propertize mime-type 'face 'consult-recoll-mime-face))))

  (setq consult-recoll-format-candidate #'my-recoll-format)

;;;;; COMPANY

(after! company
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-backends '(company-capf company-dabbrev company-files company-yasnippet)
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(use-package! company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

(set-company-backend! '(text-mode markdown-mode gfm-mode)
  '(:seperate company-ispell company-files company-yasnippet))
(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))


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
  (setq magit-diff-refine-hunk 'all)
  (setq magit-revision-show-gravatars '("^author:     " . "^commit:     ")))

;;;;; MIXED PITCH

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

(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(after! mixed-pitch
  (defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
  (setq mixed-pitch-set-height t)
  (setq variable-pitch-serif-font (font-spec :family "Ubuntu" :size 22))
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))

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

;;;;; TRAMP

(after! tramp
  (setenv "SHELL" "/bin/bash")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|
\\)[^]#$%>\n]*#?[]#$%>] *\\(�\\[[0-9;]*[a-zA-Z] *\\)*"))

;;;;; TREEMACS

(after! treemacs
  (setq +treemacs-git-mode 'extended)
  (setq treemacs-width 30))

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


;;;; DIFFTASTIC

(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))

(defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(transient-define-prefix th/magit-aux-commands ()
  "My personal auxiliary magit commands."
  ["Auxiliary commands"
   ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
   ("s" "Difftastic Show" th/magit-show-with-difftastic)])

(transient-append-suffix 'magit-dispatch "!"
  '("#" "My Magit Cmds" th/magit-aux-commands))

(define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands)


;;;; MODULES

;;;;; AGGRESSIVE INDENT

(use-package! aggressive-indent
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook    #'aggressive-indent-mode)
  (add-hook 'php-mode-hook #'aggressive-indent-mode)
  (add-hook 'hy-mode-hook #'aggressive-indent-mode))

;;;;; EBUKU

(with-eval-after-load 'ebuku
  (+evil-collection-init 'ebuku)
  (evil-collection-ebuku-setup))

;;;;; EVIL

(global-evil-matchit-mode 1)

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

;;;;; CALENDAR

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; org-agenda source
    (cfw:org-create-file-source "Calendar" (concat org-directory "/" "Calendar.org") "Orange")

    )))

;;;;; KEYCHAIN-ENVIRONMENT

(use-package! keychain-environment
  :init
  (keychain-refresh-environment))

;;;;; KHALEL

(use-package! khalel
  :commands (khalel-export-org-subtree-to-calendar
             khalel-import-upcoming-events
             khalel-edit-calender-event
             khalel-add-capture-template
             )
  :config
  (setq khalel-khal-command "/usr/bin/khal")
  (setq khalel-vdirsyncer-command "/usr/bin/vdirsyncer")
  (setq khalel-default-calendar "personal")
  (setq khalel-capture-key "e")
  (setq khalel-import-org-file (concat org-directory "/" "Calendar.org"))
  (setq khalel-import-org-file-confirm-overwrite nil)
  (setq khalel-import-time-delta "30d")
  )

;;  Added to stop Calendar.org from prompting on every startup. 

(setq safe-local-variable-values
   (quote
    ((buffer-read-only . 1))))

;;;;; KHARDEL

(use-package! khardel
  :defer t )

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


;;;;; NGINX

(use-package! company-nginx
  :after nginx-mode
  :config (add-hook 'nginx-mode-hook (lambda () (add-to-list 'company-backends #'company-nginx))))

(use-package! nginx-mode
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
  :defer t
  :mode "\\.sts\\'")

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

(add-hook 'doom-first-buffer-hook  #'marty/startwakatime)

;;;;; ZEAL-AT-POINT
(use-package! zeal-at-point
  :config
  (global-set-key "\C-cd" #'zeal-at-point)
  (add-to-list 'zeal-at-point-mode-alist '(python-mode . "python")))

;;;; LOAD FILES

(load! "keybindings.el")
(load! "functions.el")
(load! "org-mode.el")
(load! "mu4e.el")
(load! "abbrev.el")

;;; CUSTOM

(setq-default custom-file (expand-file-name "custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
