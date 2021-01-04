;; Global configurations
(tool-bar-mode 0)
(setq inhibit-startup-message t)
(global-linum-mode)
(show-paren-mode 1)

;; Straight with use-package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)


;; Theme
;; (use-package material-theme :straight t)
;; (use-package spacemacs-theme :straight t)
(straight-use-package 'spacemacs-theme)
(load-theme 'spacemacs-dark t)

;; Numerate Window
;; Switching windows:
;; (use-package ace-window :straight t)
;; (global-set-key (kbd "M-o") 'ace-window)
;; (use-package switch-window :straight t)
(use-package winum :straight t)		;C-w [number]
(winum-mode)

(use-package which-key :straight t)
(which-key-mode)

;; Helm
(use-package helm
  :straight t
  :init (helm-mode 1)
  :bind (("M-x" . helm-M-x)
	("C-x C-f" . helm-find-files)))

;; Git
(use-package magit :straight t)
(use-package projectile
  :straight t
  :ensure t
  :init (projectile-mode +1)
  :bind (( "s-p" . projectile-command-map)
	 ("C-c p" . projectile-command-map)
	 ))

;; Flycheck
(use-package flycheck
  :straight t
  :ensure t
  :init (global-flycheck-mode))

;; Python
;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
;; https://realpython.com/emacs-the-best-python-editor/
(use-package elpy
  :straight t
  :ensure t
  :init (elpy-enable))
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)) ;activate flycheck

;; (use-package pyvenv :straight t)
;; (use-package virtualenvwrapper :straight t)
;; (use-package pyenv-mode :straight t)


