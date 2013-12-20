;; No backup files please
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Python mode
(autoload 'python-mode "python-mode.el" "Python mode." t)
(setq auto-mode-alist (append '(("/*.\.py$" . python-mode)) auto-mode-alist))

;; Muttrc mode
(autoload 'muttrc-mode "muttrc-mode.el"
  "Major mode to edit muttrc files" t)
(setq auto-mode-alist (append '(("muttrl\\'" . muttrc-mode))
                              auto-mode-alist))
;; Paren matching
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Goto line number
(global-set-key "\M-g" 'goto-line)

;; Always show column number
(column-number-mode 1)

;;
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))

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


(add-hook 'c-mode-hook
          '(lambda ()
             (setq indent-tabs-mode t)
             (c-set-style "linux-tabs-only")))

; For diff
(setq diff-switches "-u")
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

; Org mode keybindings
(require 'org-install)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-reverse-note-order t)

;; Org notes
(global-set-key (kbd "C-c r") 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(setq org-remember-templates
      '((?n "* %U %?\n\n %i\n %a" "~/notes.org")))
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))




;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


;;;;org-mode configuration
;; Enable org-mode
(require 'org)
;; Make org-mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/test.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq org-log-done 'note)


(global-set-key [f12] 'iwb)
(global-set-key [f1]  'compile)
