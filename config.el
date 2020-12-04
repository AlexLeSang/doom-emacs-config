;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq fancy-splash-image "~/.doom.d/banner/emacs.png")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Oleksandr Halushko"
      user-mail-address "alexlesang@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'zenburn)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; (setq doom-font (font-spec :family "Source Code Pro" :size 15))
(setq doom-font (font-spec :family "Fira Code" :size 13))

(setq gc-cons-threshold 87777216)
(setq gc-cons-percentage 0.2)
(setq truncate-lines nil)

;; spacemacs keybindings
(setq evil-escape-key-sequence "fd")
(setq-default evil-disable-insert-state-bindings t)
(map! :i "C-y" #'yank)
(map! :i "C-k" #'kill-line)

(load! "~/.doom.d/modules/spacemacs/+spacemacs")

(after! ivy
  ;; switch to buffer
  (map! :map evil-normal-state-map :g "<C-tab>" #'switch-to-buffer)
  (map! :map evil-normal-state-map :g "gs" #'swiper)
  (setq +ivy-buffer-preview t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))))

(after! ivy-posframe
  (setq ivy-posframe-border-width 1))

;; Remove evil inhibit
(after! evil-escape
  (setq evil-escape-inhibit-functions nil))

;; Color mode
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook 'color-identifiers-mode)

;; company settings
(after! company
  (setq company-idle-delay 0.8
        company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))


(after! company-box-doc
  (setq company-box-doc-delay 1.6))

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; (after! ivy-posframe
;;   ; Set frame position
;;   (setf (alist-get t ivy-posframe-display-functions-alist)
;;         #'ivy-posframe-display-at-frame-top-center))

(add-hook 'window-setup-hook #'toggle-frame-maximized)

;; magit-todos uses hl-todo-keywords
(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("HACK"  . ,(face-foreground 'warning))
          ("TEMP"  . ,(face-foreground 'warning))
          ("DONE"  . ,(face-foreground 'success))
          ("NOTE"  . ,(face-foreground 'success))
          ("DONT"  . ,(face-foreground 'error))
          ("DEBUG"  . ,(face-foreground 'error))
          ("FAIL"  . ,(face-foreground 'error))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX"   . ,(face-foreground 'error))
          ("XXXX"  . ,(face-foreground 'error)))))

;; eshell aliases
(after! eshell
  ;; eshell-mode imenu index
  (defun eshell/l (&rest args) (eshell/ls "-l" args))
  (defun eshell/e (file) (find-file file))
  (defun eshell/md (dir) (eshell/mkdir dir) (eshell/cd dir))

  ;; This is a key-command
  (defun my/eshell-clear-keystroke ()
    "Allow for keystrokes to invoke eshell/clear"
    (interactive)
    (eshell/clear-scrollback)
    (eshell-send-input))

  (defun my/ivy-eshell-history ()
    (interactive)
    (counsel-esh-history)
    (evil-insert-state))

  (defun spacemacs/init-ivy-eshell ()
    "Initialize ivy-eshell."
    (progn
      (map! :map eshell-mode-map :in "C-l" nil)
      (map! :map eshell-mode-map :in "C-l" #'my/eshell-clear-keystroke)
      (map! :map eshell-mode-map :in "M-l" nil)
      (map! :map eshell-mode-map :in "M-l" #'my/ivy-eshell-history)))

  (add-hook 'eshell-mode-hook 'spacemacs/init-ivy-eshell))

;; evil snipe
(after! evil-snipe
  (setq evil-snipe-scope 'whole-visible))

;; lsp
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-signature-auto-activate nil
        lsp-file-watch-threshold nil
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t))

;; info-colors
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)


(use-package! vlf-setup
  :defer-incrementally vlf vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff)

(use-package abbrev
  :init
  (setq-default abbrev-mode t)
  ;; a hook funtion that sets the abbrev-table to org-mode-abbrev-table
  ;; whenever the major mode is a text mode
  (defun tec/set-text-mode-abbrev-table ()
    (if (derived-mode-p 'text-mode)
        (setq local-abbrev-table org-mode-abbrev-table)))
  :commands abbrev-mode
  :hook
  (abbrev-mode . tec/set-text-mode-abbrev-table)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir))
  (setq save-abbrevs 'silently))

;; Extend mode config
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile-build\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("conanfile.txt" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.prf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.ks\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.repo\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . js-mode))

;; (defun remove-dos-eol ()
;;   "Do not show ^M in files containing mixed UNIX and DOS line endings."
;;   (interactive)
;;   (setq buffer-display-table (make-display-table))
;;   (aset buffer-display-table ?\^M []))

;; (add-hook 'c++-mode-hook #'remove-dos-eol)

;; (require 'logview)

(after! avy
  (setq avy-all-windows 'all-frames))

(after! all-the-icons
  (setq all-the-icons-scale-factor 1.0))

;; modeline
(after! doom-modeline
  (setq doom-modeline-height 20
        doom-modeline-buffer-encoding nil
        doom-modeline-gnus nil
        doom-modeline-gnus-timer 0
        doom-modeline-irc nil))

