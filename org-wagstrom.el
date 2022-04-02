;; Patrick Wagstrom's Custom org-mode Functions
;;
;; Copyright (c) 2021 Patrick Wagstrom <patrick@wagstrom.net>
;; Licensed under terms of the MIT License

(provide 'org-wagstrom)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(defvar org-directory "~/org"
  "Default directory for org mode files")

(defvar org-person-file-name "~/org/Person - %s.org"
  "The default structure for an org file referring to a specific person")

(defvar org-person-regexp "^Person - \\(.*\\).org$"
  "Regular expression glob for finding person files")

(defvar org-company-file-name "~/org/Company - %s.org"
  "The default structure for an org file referring to a specific company")

(defvar org-company-regexp "^Company - \\(.*\\).org$"
  "Regular expression glob for finding company files")


;; org mode meeting support
(defvar org-create-meeting-list-file (concat (file-name-as-directory org-directory) "meeting_notes.org")
  "The file that contains the list of all proevious meetings")

;; paste images
(defvar org-image-directory 
  (file-truename
   (file-name-as-directory
    (concat (file-name-as-directory org-directory)
            "images")))
  "The directory that will store all of the images pasted into org-mode notes")

(define-skeleton org-company-skeleton
  "Creates a basic skeleton for a company"
  nil
  "#+TITLE: " str "\n"
  "#+STARTUP: showall\n"
  "#+CATEGORY: company\n"
  "#+TAGS: company\n"
  "#+DATE: " (progn (org-insert-time-stamp nil "HH:MM" t) nil) "\n"
  "\n"
  "* Background\n"
  "- *URL:* \n"
  "- *Category:* \n"
  "- *Primary Contact:* \n"
  "- *Funding:* \n"
  "- *Valuation:* \n"
  "- *CrunchBase URL:* \n"
  "\n"
  "* People\n"
  "\n"
  "* Meeting History\n"
  )

(define-skeleton org-person-skeleton
  "Creates a basic skeleton for a person"
  nil
  "#+TITLE: " str "\n"
  "#+STARTUP: showall\n"
  "#+CATEGORY: person\n"
  "#+TAGS: person\n"
  "#+DATE: " (progn (org-insert-time-stamp nil "HH:MM" t) nil) "\n"
  "\n"
  "* Background\n"
  "- *Company:* \n"
  "- *Role:* \n"
  "- *Start Date:* \n"
  "- *Namely Profile:* \n"
  "- *LinkedIn Profile:* \n"
  "\n"
  "\n"
  )

(define-skeleton org-meeting-skeleton
  "Creates a basic skeleton for a meeting"
  nil
  "#+TITLE: " str "\n"
  "#+STARTUP: showall\n"
  "#+CATEGORY: meeting\n"
  "#+TAGS: meeting\n"
  "#+DATE: " (progn (org-insert-time-stamp nil "HH:MM" t) nil) "\n"
  "\n"
  "* Context\n"
  "\n"
  "* Attendees\n"
  "\n"
  "* Take Aways - " str "\n"
  "\n"
  "* Notes\n"
  )


;; override spacebar in the mini buffer, I don't use it for completion
;; that often and it breaks the ability to use spaces in people's names.
;;
;; see: https://emacs.stackexchange.com/a/19831/29014
;;      https://stackoverflow.com/a/17476486/57626
;;
;; for IDO mode see: https://github.com/emacs-mirror/emacs/blob/222d033254e1c0c918f3dec523517f3192bc7086/lisp/ido.el#L211-L214

(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)
(define-key ido-common-completion-map " " 'self-insert-command)

(defun org-id-get-id-from-file (file)
  "Gets the ID out of a file"
  (interactive)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (setq outid (org-id-get))
    )
  outid)

;; helper function to get the list of people that have defined files
;; already in org-mode. This is use primarily so we can get completion
;; when using org-insert-person
(defun org-get-regexp-files (file-dir file-regexp)
  (progn
    (setq rv ())
    (setq df (directory-files file-dir))
    (dolist (elem df rv)
      (if (string-match file-regexp elem)
	  (setq rv (cons (match-string 1 elem) rv))))
    (sort rv #'string-collate-lessp)))
  
(defun org-get-person-files ()
  (org-get-regexp-files org-directory org-person-regexp))

(defun org-get-company-files ()
  (org-get-regexp-files org-directory org-company-regexp))

(defun org-insert-entity (link-file-name link-text body-func body-args)
  "Generalized wrapper function for inserting entities in org-mode"
  (interactive)
  (progn
    (setq working-buffer (current-buffer))
    (if (not (file-exists-p link-file-name))
	(progn
	  (setq new-buf-name " org-insert-entity-tempbuffer")
	  (setq new-buf (generate-new-buffer new-buf-name))
	  (set-buffer new-buf)
	  (funcall body-func body-args)
	  (write-file link-file-name)
	  (write-file link-file-name)
	  (kill-buffer new-buf)))
    (set-buffer working-buffer)
    (setq entity-link-text (format "[[id:%s][%s]]" (org-id-get-id-from-file link-file-name) link-text))
    (insert entity-link-text)))

(defun org-insert-company (company-name)
  "Inserts a link to a company document.
  COMPANY-NAME should be the full ame of the person to create a link to.
  If the document for COMPANY-NAME does not exist then it is created."
  (interactive
   (list
    (completing-read "Company Name: " (org-get-company-files) nil nil)))
  (setq company-file-name (format org-company-file-name company-name))
  (org-insert-entity company-file-name company-name #'(lambda (str) (org-company-skeleton str)) company-name))


(defun org-insert-person (person-name)
  "Inserts a link to a person document.
  PERSON-NAME should be the full ame of the person to create a link to.
  If the document for PERSON-NAME does not exist then it is created."
  (interactive
   (list
    (completing-read "Person Name: " (org-get-person-files) nil nil)))
  (setq person-file-name (format org-person-file-name person-name))
  (org-insert-entity person-file-name person-name #'(lambda (str) (org-person-skeleton str)) person-name))


(defun org-wagstrom-save-image-from-clipboard (filename)
  "Saves an image from the clipboard to a specific path
  FILENAME should be the destination filename for the image."
  (interactive)
  (if (file-exists-p "/usr/local/bin/pngpaste")
      (shell-command (concat "/usr/local/bin/pngpaste \"" file-name-with-path "\"") nil nil))
  (if (file-exists-p "/home/pwagstro/.local/bin/save_image_from_clipboard")
      (shell-command (concat "/home/pwagstro/.local/bin/save_image_from_clipboard \"" file-name-with-path "\"") nil nil)))


(defun org-insert-image ()
  "Pastes an image into a file and then links the image in org-mode"
  (interactive)
  (setq file-name (concat (format-time-string "%Y%m%d %H%M%s") " - " (buffer-name) ".png"))
  (setq file-name-with-path (concat org-image-directory file-name))
  (org-wagstrom-save-image-from-clipboard file-name-with-path)
  (insert "#+CAPTION: Your_Caption_Here")
  (newline)
  (insert "#+ATTR_ORG: :width 500") 
  (newline)
  (insert (concat "[[" file-name-with-path "]]"))
  (newline)
  )


 
;; TODO this should check to see if the meeting already exists and, if so, just open it
(defun org-create-meeting (meeting-name)
  (interactive "sMeeting Name:")
  (setq meeting-name-with-date
	(concat (format-time-string "%Y%m%d")
		" - "
		meeting-name))
  (setq meeting-short-filename
	(replace-regexp-in-string
	 "/"
	 ""
	 (concat meeting-name-with-date
		".org")))
  (setq filename
	(concat
	 (file-name-as-directory org-directory)
	 meeting-short-filename))
  (setq meeting-link-text (format "\n* [[file:%s][%s]]" meeting-short-filename meeting-name-with-date))
  (message "Meeting name: %s" filename)

  (setq meeting-list-buffer (get-buffer (file-name-nondirectory org-create-meeting-list-file)))

  ;; add the entry to the index file
  (if (buffer-live-p meeting-list-buffer)
      (with-current-buffer meeting-list-buffer
	(progn (goto-char (point-max))
               (insert meeting-link-text)
               (save-buffer)
	       ))
    (write-region meeting-link-text nil org-create-meeting-list-file 'append)
    )

  (with-current-buffer (find-file filename)
    (org-meeting-skeleton meeting-name-with-date))
  )


;; see: https://stackoverflow.com/a/16247032/57626
(defun my/org-add-ids-to-headlines-in-file ()
  "Add ID properties to all headlines in the current file which
do not already have one."
  (interactive)
  ;; we need to save twice because otherwise we sometimes get "Non-existent agenda file" errors
  ;; we can't just check if the file exists, beacuse that will result in infinite recursion.
  ;; instead, we check to see if it's got an id already.
  (if (file-exists-p (buffer-file-name))
      (progn
	(save-excursion
	  (goto-char (point-min))
	  (org-id-get-create))

	(org-map-entries 'org-id-get-create))))
  ;; this blob saves the cursor, goes to the beginning, and creates an id for the org file if needed

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file nil 'local)))
