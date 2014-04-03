;; No backup files please
(setq make-backup-files nil)
(setq auto-save-default nil)

(add-to-list 'load-path "~/.emacs.d/twittering-mode")
(require 'twittering-mode)



;; Paren matching
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; 80 columns should be max - however highlight lines longer than 120 columns
(custom-set-faces
 '(my-trailing-space-face ((((class color)) (:background "grey20"))) t)
 '(my-long-line-face ((((class color)) (:background "brightred"))) t))


(add-hook 'font-lock-mode-hook
	  (function
	   (lambda ()
	     (setq font-lock-keywords
		   (append font-lock-keywords
			   '(("\t+" (0 'my-tab-face t))
			     ("^.\\{141,\\}$" (0 'my-long-line-face t))))))))

;; delete trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Always show column number
(column-number-mode 1)


;; move between windows with shift+cursor key
;; fbound for xemacs which doesnt have it
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;;
;;(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))


(global-font-lock-mode 1)

;; Linux kernel C Mode as from src/linux/Documentation/CodingStyle Chap 9.
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Add kernel style
	    (c-add-style
	     "linux-tabs-only"
	     '("linux" (c-offsets-alist
			(arglist-cont-nonempty
			 c-lineup-gcc-asm-reg
			 c-lineup-arglist-tabs-only))))))

;; !! depends on directory of linux source tree(s) !!
(add-hook 'c-mode-hook
	  (lambda ()
	    (let ((filename (buffer-file-name)))
	      ;; enable kernel mode for appropriate files
	      ( when (and filename
			  (string-match (expand-file-name "~/src/linux-trees")
					filename))
		(setq indent-tabs-mode t)
		(c-set-style "linux-tabs-only")))))


;; Enable transient mark mode
;(transient-mark-mode 1)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


;; =========================
;; org-mode configuration
;; =========================

;; Enable org-mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-files (list "~/org/privat.org"
			     "~/org/arbeit.org"
			     "~/org/diplomarbeit.org"))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq org-log-done 'note)


;; ===============
;; twittering-mode
;; ===============

(setq twittering-use-master-password t)
(setq twittering-cert-file "/etc/ssl/certs/ca-bundle.crt")


;(setq twittering-initial-timeline-spec-string
;      '(":home"
;        ":replies"
;        ":favorites"
;        ":direct_messages"))
(setq twittering-icon-mode t)
(setq twittering-timer-interval 300)
(setq twittering-tinyurl-service 'bit.ly)
(setq twittering-bitly-login "o_7njvl7i91o")
(setq twittering-bitly-api-key "R_02e677dbe510483ea16bd05588679151")
(add-hook 'twittering-edit-mode-hook (lambda () (ispell-minor-mode) (flyspell-mode)))


;; compilation - possibly find Makefile in parent dir(s)
;;(setq compilation-filenames '("Makefile" "makefile"))
(setq compilation-filenames '("Makefile.emacs"))
(defun get-nearest-makefile ()
  "Search for the makefile traversing up the directory tree."
  (let ((dir default-directory)
	(parent-dir (file-name-directory (directory-file-name default-directory)))
	(nearest-compilation-file 'nil))
    (while (and (not (string= dir parent-dir))
		(not nearest-compilation-file))
      (dolist (filename compilation-filenames)
	(setq file-path (concat dir filename))
	(when (file-readable-p file-path)
	  (setq nearest-compilation-file file-path)))
      (setq dir parent-dir
	    parent-dir (file-name-directory (directory-file-name parent-dir))))
    nearest-compilation-file))

;; KEYMAPS
(global-set-key [f12] 'iwb)         ;; reindent complete buffer

;; call make
(global-set-key [f1]  (lambda () (interactive) (compile (format "make -f %s" (get-nearest-makefile)))))


(global-set-key "\M-g" 'goto-line)  ;; Goto line number
