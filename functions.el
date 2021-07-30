;;; functions.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2021, Marty Buchaus, all rights reserved.
;; Created:  7 July 2021
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Functions

;;;;; Roam Daily Functions

(defun marty/org-roam-dailies-graphicslink ()
  " Set the Graphics Link to Today in the Pictures folder that maid pushes to."
  (interactive)
  (let* ((year  (string-to-number (substring (buffer-name) 0 4)))
         (month (string-to-number (substring (buffer-name) 5 7)))
         (day   (string-to-number (substring (buffer-name) 8 10)))
         (datim (encode-time 0 0 0 day month year)))
    (format-time-string "[[/home/marty/Nextcloud/Pictures/2020 - 2029/%Y/%0m/Daily/%d][Graphics Link]]" datim)))

(defun marty/org-roam-dailies-title ()
  (interactive)
  (let* ((year  (string-to-number (substring (buffer-name) 0 4)))
         (month (string-to-number (substring (buffer-name) 5 7)))
         (day   (string-to-number (substring (buffer-name) 8 10)))
         (datim (encode-time 0 0 0 day month year)))
    (format-time-string "%A, %B %d %Y" datim)))

;;;;; Roam Extra

(defun roam-extra:get-filetags ()
  (split-string (or (org-roam-get-keyword "filetags") "")))

(defun roam-extra:add-filetag (tag)
  (let* ((new-tags (cons tag (roam-extra:get-filetags)))
         (new-tags-str (combine-and-quote-strings new-tags)))
    (org-roam-set-keyword "filetags" new-tags-str)))

(defun roam-extra:del-filetag (tag)
  (let* ((new-tags (seq-difference (roam-extra:get-filetags) `(,tag)))
         (new-tags-str (combine-and-quote-strings new-tags)))
    (org-roam-set-keyword "filetags" new-tags-str)))

(defun roam-extra:todo-p ()
  "Return non-nil if current buffer has any TODO entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h)
          'todo))
    nil 'first-match))

(defun roam-extra:update-todo-tag ()
  "Update TODO tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (org-roam-file-p))
    (org-with-point-at 1
      (let* ((tags (roam-extra:get-filetags))
             (is-todo (roam-extra:todo-p)))
        (cond ((and is-todo (not (seq-contains-p tags "todo")))
               (roam-extra:add-filetag "todo"))
              ((and (not is-todo) (seq-contains-p tags "todo"))
               (roam-extra:del-filetag "todo")))))))

(defun roam-extra:todo-files ()
  "Return a list of roam files containing todo tag."
  (org-roam-db-sync)
  (let ((todo-nodes (seq-filter (lambda (n)
                                  (seq-contains-p (org-roam-node-tags n) "todo"))
                                (org-roam-node-list))))
    (seq-uniq (seq-map #'org-roam-node-file todo-nodes))))

(defun roam-extra:update-todo-files (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (roam-extra:todo-files)))


;;;;; Open file Functions

(defun mb/0mobile ()
  (interactive) (find-file (concat org-directory "0mobile.org")))

(defun mb/desktop ()
  (interactive) (find-file (concat org-directory "desktop.org")))

(defun mb/contacts ()
  (interactive) (find-file (concat org-directory "contacts.org")))

(defun mb/Tasks ()
  (interactive) (find-file (concat org-directory "Tasks.org")))

(defun mb/Habits ()
  (interactive) (find-file (concat org-directory "Habits.org")))

(defun mb/read-later ()
  (interactive) (find-file (concat org-directory "read-later.org")))

(defun mb/Someday ()
  (interactive) (find-file (concat org-directory "Someday.org")))

(defun mb/TipJar ()
  (interactive) (find-file (concat org-directory "TipJar/index.org")))

(defun mb/base-keybinding ()
  (interactive) (find-file "~/.config/doom/keybindings.el"))

(defun mb/base-config ()
  (interactive) (find-file "~/.config/doom/config.el"))

(defun mb/org-config ()
  (interactive) (find-file "~/.config/doom/org-mode.el"))

(defun mb/functions ()
  (interactive) (find-file "~/.config/doom/functions.el"))

(defun mb/packages ()
  (interactive) (find-file "~/.config/doom/packages.el"))

;;;;; Autoinsert yas expand

(defun marty/autoinsert-yas-expand ()
  (let ((template ( buffer-string )))
    (delete-region (point-min) (point-max))
    (yas-expand-snippet template)
    (evil-insert-state)))

;;;;; Org Functions
;;;;;; publish functions

(defun marty/publish (a b c)
  (setq org-export-with-toc t)
  (org-html-publish-to-html a b c)
  (setq org-export-with-toc nil)
  (org-ascii-publish-to-ascii a b c))


;;;;;; Open Mutt Message

(defun mutt-open-message (message-id)
  "In neomutt, open the email with the the given Message-ID"
  (let*
      ((message-id
        (replace-regexp-in-string "^/*" "" message-id))
       (mail-file
        ;; notmuch
        (replace-regexp-in-string
         "\n$" "" (shell-command-to-string
                   (format "notmuch search --output=files id:%s" message-id))))
       (mail-box (replace-regexp-in-string "/home/marty/Mail" "" mail-file))
       (mail-dir (replace-regexp-in-string "/\\(cur\\|new\\|tmp\\)/$" ""
                                           (file-name-directory mail-box)))
       (mutt-keystrokes
        (format "macro index - l~i%s; push -\\nb\\n" (shell-quote-argument message-id)))
       (mutt-command (format "neomutt -f '=%s' -e '%s'" mail-dir  mutt-keystrokes)))
    ;; MU
    ;;         (replace-regexp-in-string
    ;;          "\n$" "" (shell-command-to-string
    ;;                    (format "mu find -u i:%s --fields 'm'" message-id ))))
    ;;        (mutt-keystrokes
    ;;         (format "macro index - l~i%s; push -\\nb\\n" (shell-quote-argument message-id)))
    ;;         (mutt-command (format "neomutt -f '=%s' -e '%s'" mail-file  mutt-keystrokes)))

    (message "Launching neomutt for message %s" message-id)
    (message " %s" mutt-command)
    (call-process "setsid" nil nil nil
                  "-f" "termite" "-e"
                  mutt-command)))

;;;;;; MU4E

(defvar marty-mu4e/mu4e-compose-signed-p t)
(defvar marty-mu4e/mu4e-compose-signed-and-crypted-p nil)

(defun marty-mu4e/mu4e-compose-maybe-signed-and-crypted ()
  "Maybe sign or encrypt+sign message.
Message is signed or encrypted+signed when replying to a signed or encrypted
message, respectively.
Alternatively, message is signed or encrypted+signed if
`ambrevar/mu4e-compose-signed-p' or `ambrevar/mu4e-compose-signed-and-crypted-p' is
non-nil, respectively.
This function is suitable for `mu4e-compose-mode-hook'."
  (let ((msg mu4e-compose-parent-message))
    (cond
     ((or marty-mu4e/mu4e-compose-signed-and-crypted-p
          (and msg (member 'encrypted (mu4e-message-field msg :flags))))
      (mml-secure-message-sign-encrypt))
     ((or marty-mu4e/mu4e-compose-signed-p
          (and msg (member 'signed (mu4e-message-field msg :flags))))
      (mml-secure-message-sign-pgpmime)))))

;; Follow up quick key

(defun marty/capture-mail-follow-up (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture "ma"))

(defun marty/capture-mail-read-later (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture "mr"))

;; End MU4E
