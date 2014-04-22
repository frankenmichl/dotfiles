;; ===================
;; load required files
;; ===================
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ecb")
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'ecb)
(require 'w3m-load)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)


;; No backup files please
(setq make-backup-files nil)
(setq auto-save-default nil)


;; Font Lock Mode
;; ==============
(global-font-lock-mode 0)

(font-lock-add-keywords
 'c-mode
 '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))
(font-lock-add-keywords
 'c-mode
 '(("\\<\\(TODO\\):" 1 font-lock-warning-face t)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
(add-hook 'cc-mode-hook 'turn-on-font-lock)
(add-hook 'c-mode-hook 'turn-on-font-lock)
(add-hook 'c++-mode-hook 'turn-on-font-lock)
(add-hook 'java-mode-hook 'turn-on-font-lock)
(add-hook 'asm-mode-hook 'turn-on-font-lock)
(add-hook 'sh-mode-hook 'turn-on-font-lock)
(add-hook 'shell-script-mode-hook 'turn-on-font-lock)
(add-hook 'makefile-mode-hook 'turn-on-font-lock)
(add-hook 'perl-mode-hook 'turn-on-font-lock)
(add-hook 'python-mode-hook 'turn-on-font-lock)
(add-hook 'xrdb-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'ps-mode-hook '(lambda ()
                           (make-local-variable 'font-lock-support-mode)
                           (make-local-variable 'lazy-lock-defer-on-scrolling)
                           (setq font-lock-support-mode 'lazy-lock-mode
                                 lazy-lock-defer-on-scrolling t)
                           (turn-on-font-lock)))
(add-hook 'ps-run-mode-hook '(lambda () (turn-on-font-lock)))

;; Modes
;; =====
;; Mode Options
;; ============

(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode"   t)
(autoload 'objc-mode "cc-mode" "Objective C Editing Mode" t)
(autoload 'text-mode "indented-text-mode" "Indented Text Editing Mode" t)
(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)
(autoload 'ps-mode "ps-mode" "Major mode for editing PostScript" t)
(setq auto-mode-alist
      (append '(("\\.C$"       . c++-mode)
                ("\\.cc$"      . c++-mode)
                ("\\.c$"       . c-mode)
                ("\\.h$"       . c++-mode)
                ("\\.i$"       . c++-mode)
                ("\\.ii$"      . c++-mode)
                ("\\.m$"       . objc-mode)
                ("\\.pl$"      . perl-mode)
                ("\\.sql$"     . c-mode)
                ("\\.sh$"      . shell-script-mode)
                ("\\.mak$"     . makefile-mode)
                ("\\.GNU$"     . makefile-mode)
                ("makefile$"   . makefile-mode)
                ("Makefile$"   . makefile-mode)
                ("Imakefile$"  . makefile-mode)
                ("\\.Xdefaults$"    . xrdb-mode)
                ("\\.Xenvironment$" . xrdb-mode)
                ("\\.Xresources$"   . xrdb-mode)
                ("*.\\.ad$"         . xrdb-mode)
                ("\\.[eE]?[pP][sS]$" . ps-mode)
                ) auto-mode-alist))


;; Text Options
;; ============
(setq-default fill-column 70)


;; Paren matching
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; 80 columns should be max - however highlight lines longer than 120 columns
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(my-long-line-face ((((class color)) (:background "brightred"))) t)
 '(my-trailing-space-face ((((class color)) (:background "grey20"))) t))

(add-hook 'font-lock-mode-hook
          (function
           (lambda ()
             (setq font-lock-keywords
                   (append font-lock-keywords
                           '(("\t+" (0 'my-tab-face t))
                             ("^.\\{121,\\}$" (0 'my-long-line-face t))))))))

;; delete trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Always show column number
(column-number-mode 1)


;;
;;(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))


;; ======
;; C-Mode
;; ======
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

;; c refactoring
(require 'c-refactor)
(add-hook 'c-mode-hook 'c-refactor-mode-launch)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; ======================
;; org-mode configuration
;; ======================

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-files (list "~/org/work.org"
                             "~/org/diplomarbeit.org"
                             "~/org/home.org"))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))



(setq org-log-done t)



;; compilation - possibly find Makefile in parent dir(s)
(setq compilation-filenames '("Makefile" "makefile"))
;;(setq compilation-filenames '("Makefile.emacs"))
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

;; ==============================
;; emacs shell mode configuration
;; ==============================
(setq explicit-shell-file-name "/usr/bin/bash")
(setq shell-file-name "bash")
(setq explicit-bash-args '("--login" "-i"))
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(setq comint-prompt-read-only t)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; =================
;; hide menu bar etc
;; =================
                                        ;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(inhibit-startup-screen t))



;; ============
;; Key Bindings
;; ============




(global-set-key [f1]  (lambda () (interactive) (compile (format "make -f %s" (get-nearest-makefile))))) ;; call make
(global-set-key [f3] 'ispell-buffer)  ;; spell check current buffer
(global-set-key [f5] 'eshell) ;; launch terminal
(global-set-key [f12] 'iwb)           ;; reindent complete buffer

(global-set-key "\M-g" 'goto-line)  ;; Goto line number
