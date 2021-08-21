;; Patrick Wagstrom's Custom org-mode Functions
;;
;; Copyright (c) 2021 Patrick Wagstrom <patrick@wagstrom.net>
;; Licensed under terms of the MIT License

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(define-skeleton org-person-skeleton
  "Creates a basic skeleton for a person"
  nil
  "#+TITLE: " str "\n"
  "#+STARTUP: showall\n"
  "#+CATEGORY: person\n"
  "#+TAGS: person\n"
  "#+DATE: <" (format-time-string "%Y-%m-%d %a %H:%M %Z") ">\n"
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

(setq-default org-directory "~/org")
(setq-default org-person-regexp "^Person - \\(.*\\).org$")


;; override spacebar in the mini buffer, I don't use it for completion
;; that often and it breaks the ability to use spaces in people's names.
;;
;; see: https://emacs.stackexchange.com/a/19831/29014
;;      https://stackoverflow.com/a/17476486/57626
;;
;; for IDO mode see: https://github.com/emacs-mirror/emacs/blob/222d033254e1c0c918f3dec523517f3192bc7086/lisp/ido.el#L211-L214

(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)
(define-key ido-common-completion-map " " 'self-insert-command)

;; helper function to get the list of people that have defined files
;; already in org-mode. This is use primarily so we can get completion
;; when using org-insert-person
(defun org-get-person-files ()
  (progn
    (setq rv ())
    (setq df (directory-files org-directory))
    (dolist (elt df rv)
      (if (string-match org-person-regexp elt)
	  (setq rv (cons (match-string 1 elt) rv))))
    (sort rv #'string-collate-lessp)
    ))

(defun org-insert-person (person-name)
  (interactive
   (list
    (completing-read "Person Name: " (org-get-person-files) nil nil)))
  (progn
    (setq person-file-name (format "~/org/Person - %s.org" person-name))
    (setq person-link-text (format "[[file:%s][%s]]" person-file-name person-name))
    (setq working-buffer (current-buffer))
    (if (not (file-exists-p person-file-name))
	(progn
	  ;; name for new buffer. If start with space, undo is disabled
	  (setq newBufName " xyz")

	  ;; create a new buffer, save it to a var, so later you can switch to it or kill it
	  (setq newBuf (generate-new-buffer newBufName))
	  
	  ;; make it current (but does not make it visible), so all insert etc operations works on it.
	  (set-buffer newBuf)
	  (org-person-skeleton person-name)
	  ;; like “Save As”. Save current buffer, close it, and open the new saved
	  (write-file person-file-name)	  
	  ;; close it
	  (kill-buffer newBuf)
	  ))
    (set-buffer working-buffer)
    (insert person-link-text)))

