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

(after! company
  (setq company-idle-delay 0.15)
  (setq company-minimum-prefix-length 3))

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
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t))
