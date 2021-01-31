;; Global configurations
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(setq use-dialog-box '())

(setq use-package-verbose t)
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

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(setq settings-dir  (expand-file-name "settings" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory ))))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; create empy file if it not exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Font & Geometry
(dolist (opt '((font . "Inconsolata-14")
	       (line-spacing . 0)
	       (width . 90) ))
  (add-to-list 'default-frame-alist opt))
;; (if                                     ; for large and wide descktop
;;     (= (display-pixel-width) 1920)
;;     (add-to-list 'default-frame-alist '(font . "Inconsolata-10")))

;; (dolist (opt '((font . "Inconsolata-10") (line-spacing . 0) (width . 84) ))
;;   (add-to-list 'default-frame-alist opt))
;; (require 'highlight-beyond-fill-column)
;; (setq-default fill-column 80)

;; (add-hook 'prog-mode-hook 'highlight-beyond-fill-column)
;; (custom-set-faces '(highlight-beyond-fill-column-face
;;                     ((t (:foreground "red" )))))
;; (add-hook 'prog-mode-hook 'highlight-beyond-fill-column)

;; Theme
;; (use-package material-theme :straight t)
(use-package spacemacs-theme
  :straight t
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t))
;; (straight-use-package 'spacemacs-theme)
;; (load-theme 'spacemacs-dark t)

;; Numerate Window
;; Switching windows:
;; (use-package ace-window :straight t)
;; (global-set-key (kbd "M-o") 'ace-window)
;; (use-package switch-window :straight t)

(use-package highlight-beyond-fill-column
  :load-path "lisp")

(use-package use-ttf
  :straight t
  :init
  (setq use-ttf-default-ttf-fonts
	(directory-files
	 (expand-file-name "fonts" user-emacs-directory)
	 t "\\.[ot]tf"))
  (use-ttf-install-fonts))

(use-package zoom
  :straight t
  :config
  (zoom-mode t)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618)))
  )

(use-package winum
  :straight t				; C-w [number]
  :ensure t
  :init
  (winum-mode)
  :bind (:map winum-keymap
	 ("C-`" . winum-select-window-by-number)
	 ("M-1" . winum-select-window-1)
	 ("M-2" . winum-select-window-2)
	 ("M-3" . winum-select-window-3)
	 ("M-4" . winum-select-window-4)
	 ("M-5" . winum-select-window-5)
	 ("M-6" . winum-select-window-6)
	 ("M-7" . winum-select-window-7)
	 ("M-8" . winum-select-window-8)
	 ("M-9" . winum-select-window-9)
	 ("M-0" . winum-select-window-0-or-10)))

;; Parentes
(show-paren-mode)
;; (electric-pair-mode)
(use-package rainbow-delimiters
  :straight t
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-2-face ((t (:foreground "magenta"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "tomato"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "gold")))))
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :straight t
  :init
  (require 'smartparens-config)
  :hook (prog-mode . smartparens-mode))

;; Line & Column numbers
(global-linum-mode)
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(line-number-mode)
(column-number-mode)

;; Save point position between sessions
(use-package saveplace
  :straight t
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" user-emacs-directory)))

;; Automatic key binding help
(use-package which-key
  :straight t
  :ensure t
  :init (which-key-mode))

;; Helm
(use-package helm
  :straight t
  :ensure t
  :init (helm-mode)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-." . helm-imenu)
	 ("C-x r I". helm-register)))

(use-package helm-projectile
  :straight t
  :ensure t
  :requires (helm prjectile)
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))
(use-package helm-ag
  :straight t
  :config
  ;; (custom-set-variables
  ;;  '(helm-follow-mode-persistent t))
  )
;; Git
(use-package magit :straight t :ensure t)
(use-package projectile
  :straight t
  :ensure t
  :init (projectile-mode)
  :bind (( "s-p" . projectile-command-map)
	 ("C-c p" . projectile-command-map)))

;; Flycheck
(use-package flycheck
  :straight t
  :ensure t
  :init (global-flycheck-mode))

;; Spell checking in commentary
(use-package flyspell
  :straight t
  :after flycheck
  :init
  :hook (prog-mode . flyspell-prog-mode))

(use-package ispell
  :init
  (setq  ispell-program-name "hunspell"))

;; Snippets
(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode))
(use-package yasnippet-snippets :straight t)


;; Python language
;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
;; https://realpython.com/emacs-the-best-python-editor/
(use-package ein :straight t :ensure t)
(use-package elpy
  :straight (elpy :fork (:host github :repo "gfederix/elpy" :branch "dev"))
  :config
  (elpy-enable)
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)) ;activate flycheck
  (add-hook
   'elpy-mode-hook
   (lambda () (local-set-key (kbd "M-.") 'elpy-goto-definition))))

(use-package pyvenv :straight t :ensure t)

(use-package auto-virtualenv :straight t :ensure t
  :requires (pyvenv)
  :hook ((python-mode .  auto-virtualenv-set-virtualenv)
	 (window-configuration-change . auto-virtualenv-set-virtualenv)
	 (focus-in .  auto-virtualenv-set-virtualenv)
	 ))

(use-package company
  :straight t
  :bind
  (:map company-active-map
	("C-n" . company-select-next)
	("C-p" . company-select-previous))
  :hook
  (prog-mode . company-mode)
  ;;  (setq company-idle-delay 0
  ;;      company-minimum-prefix-length 2
  ;;      company-show-numbers t
  ;;      company-tooltip-limit 10
  ;;      company-tooltip-align-annotations t
  ;;      ;; invert the navigation direction if the the completion popup-isearch-match
  ;;      ;; is displayed on top (happens near the bottom of windows)
  ;;      company-tooltip-flip-when-above t)
   )

(use-package helm-company
  :straight t
  :config
  (progn
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company)))

(use-package company-quickhelp
  ;; Quickhelp may incorrectly place tooltip towards end of buffer
  ;; See: https://github.com/expez/company-quickhelp/issues/72
  :straight t
  :config
  (company-quickhelp-mode))

(use-package polymode
  ;; https://www.masteringemacs.org/article/polymode-multiple-major-modes-how-to-use-sql-python-in-one-buffer
  :straight t
  :mode ("\.py$" . poly-python-sql-mode)
  :config
    (setq polymode-prefix-key (kbd "C-c n"))
  (define-hostmode poly-python-hostmode :mode 'python-mode)

  (define-innermode poly-sql-expr-python-innermode
    :mode 'sql-mode
    :head-matcher (rx "r" (= 3 (char "\"'")) (* (any space)))
    :tail-matcher (rx (= 3 (char "\"'")))
    :head-mode 'host
    :tail-mode 'host)

  (defun poly-python-sql-eval-chunk (beg end msg)
    "Calls out to `sql-send-region' with the polymode chunk region"
    (sql-send-region beg end))

  (define-polymode poly-python-sql-mode
    :hostmode 'poly-python-hostmode
    :innermodes '(poly-sql-expr-python-innermode)
    (setq polymode-eval-region-function #'poly-python-sql-eval-chunk)
    (define-key poly-python-sql-mode-map (kbd "C-c C-c") 'polymode-eval-chunk))

  ;; Bug? Fix polymode kill chunk so it works.
  (defun polymode-kill-chunk ()
    "Kill current chunk."
    (interactive)
    (pcase (pm-innermost-span)
      (`(,(or `nil `host) ,beg ,end ,_) (delete-region beg end))
      (`(body ,beg ,_ ,_)
       (goto-char beg)
       (pm--kill-span '(body))
       ;; (pm--kill-span '(head tail))
       ;; (pm--kill-span '(head tail))
       )
      (`(tail ,beg ,end ,_)
       (if (eq beg (point-min))
           (delete-region beg end)
         (goto-char (1- beg))
         (polymode-kill-chunk)))
      (`(head ,_ ,end ,_)
       (goto-char end)
       (polymode-kill-chunk))
      (_ (error "Canoot find chunk to kill")))))

(defun upcase-sql-keywords ()
  (interactive)
  (save-excursion
    (dolist (keywords sql-mode-postgres-font-lock-keywords)
      (goto-char (point-min))
      (while (re-search-forward (car keywords) nil t)
        (goto-char (+ 1 (match-beginning 0)))
        (when (eql font-lock-keyword-face (face-at-point))
          (backward-char)
          (upcase-word 1)
          (forward-char))))))

(use-package sqlformat
  :straight t
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

(use-package imenu-list
  :straight t
  :bind ("C-'" .  #'imenu-list-smart-toggle))


;; R language
(use-package ess
  :straight t)

(use-package ess-smart-equals
  :straight t
  :init   (setq ess-smart-equals-extra-ops '(brace paren percent))
  :after  (:any ess-r-mode inferior-ess-r-mode ess-r-transcript-mode)
  :config (ess-smart-equals-activate))

(use-package ess-R-data-view
  :straight t)
