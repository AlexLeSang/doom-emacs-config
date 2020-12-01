;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-zenburn)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

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
(setq doom-font (font-spec :family "Source Code Pro" :size 14))

;; spacemacs keybindings
(setq evil-escape-key-sequence "fd")
(setq-default evil-disable-insert-state-bindings t)
(map! :i "C-y" #'yank)
(map! :i "C-k" #'kill-line)

(load! "~/.doom.d/modules/spacemacs/+spacemacs")

;; eshell history (inspired by the one in spacemacs)
(defun spacemacs/ivy-eshell-history ()
  (interactive)
  (counsel-esh-history)
  (evil-insert-state))

(defun eshell/clear ()
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; This is a key-command
(defun spacemacs/eshell-clear-keystroke ()
  "Allow for keystrokes to invoke eshell/clear"
  (interactive)
  (eshell/clear))

(defun spacemacs/init-ivy-eshell ()
  "Initialize ivy-eshell."
  (progn
   (map! :map eshell-mode-map :in "C-l" nil)
   (map! :map eshell-mode-map :in "C-l" #'spacemacs/eshell-clear-keystroke)
   (map! :map eshell-mode-map :in "M-l" nil)
   (map! :map eshell-mode-map :in "M-l" #'spacemacs/ivy-eshell-history))
  )

(after! ivy
  (add-hook 'eshell-mode-hook 'spacemacs/init-ivy-eshell))

;; switch to buffer
(after! ivy
  (map! :map evil-normal-state-map :g "<C-tab>" #'switch-to-buffer)
  (map! :map evil-normal-state-map :g "gs" #'swiper))

;; Remove evil inhibit
(after! evil-escape
  (setq evil-escape-inhibit-functions nil))

;; Color mode
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook 'color-identifiers-mode)

(after! company
  (setq company-idle-delay 0.15)
  (setq company-minimum-prefix-length 3))

(after! ivy
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))))
