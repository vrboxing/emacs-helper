;;; eh-misc.el --- Tumashu's emacs configuation

;; * Header
;; Copyright (c) 2011-2016, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * 简介                                                  :README:
;;  这个文件是tumashu个人专用的emacs配置文件，emacs中文用户可以参考。

;;; Code:

;; * 代码                                                      :code:

;; ** cyberpunk-theme
(use-package cyberpunk-theme
  :config
  (add-hook 'after-init-hook
            #'(lambda ()
                (load-theme 'cyberpunk t)
                ;; Adjust cyberpunk theme
                (set-face-attribute 'font-lock-comment-face nil :italic nil)
                (set-face-attribute 'org-agenda-date-today nil :slant 'normal))))

;; ** Eshell
(use-package eshell
  :bind (("C-x c" . eshell))
  :ensure nil
  :config
  (use-package em-term :ensure nil)
  (use-package em-unix :ensure nil)
  (setq eshell-visual-commands
        (append '("top" "htop" "aptitude" "mutt"
                  "nano" "crontab" "vim" "less" "zile")
                eshell-visual-commands))
  (setq eshell-visual-subcommands
        (list (append '("sudo") eshell-visual-commands)
              '("git" "log" "diff" "show" "grep"
                "commit" "rebase" "pull" "push")))
  (setq eshell-visual-options
        '(("git" "--help" "--paginate")))
  (defun eh-eshell (&optional arg)
    (interactive)
    ;; 使用eshell-exec-visual第一次打开term时，
    ;; 不能使用multi-term的键盘绑定，原因不知，
    ;; 首先运行一下less, 从而让multi-term的键盘绑定生效。
    (eshell-command "less")
    (eshell arg)))

;; ** EWW
(use-package eww
  :commands eww
  :ensure nil
  :config
  (setq shr-width 90)
  ;; 搜狗:  http://www.sogou.com/sogou?query=
  ;; 百度:  http://m.baidu.com/ssid=0/s?word=
  ;; 必应:  http://cn.bing.com/search?q=
  (setq eww-search-prefix "http://www.sogou.com/sogou?query="))

;; ** 设置拼音输入法
(use-package pyim
  :ensure nil
  :config

  (setq default-input-method "pyim")

  ;; 使用全拼
  (setq pyim-default-scheme 'quanpin)

  ;; pyim 探针设置
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 pupup 来绘制选词框。
  (setq pyim-page-tooltip 'popup)

  ;; 显示5个候选词。
  (setq pyim-page-length 5)

  ;; emacs 启动时加载 pyim 词库
  ;; (add-hook 'emacs-startup-hook
  ;;           #'(lambda ()
  ;;               (pyim-restart-1 t)))

  :bind
  (("M-j" . pyim-convert-code-at-point)
   ("C-;" . pyim-delete-word-from-personal-buffer)))

(use-package pyim-basedict
  :after pyim
  :config (pyim-basedict-enable))

;; ** cnfonts
(use-package cnfonts
  :if (display-graphic-p)
  :init (setq cnfonts-verbose nil)
  :config
  (setq cnfonts-use-face-font-rescale
        (eq system-type 'gnu/linux))
  (cnfonts-enable)
  :bind (("C--" . cnfonts-decrease-fontsize)
         ("C-=" . cnfonts-increase-fontsize)
         ("C-+" . cnfonts-next-profile)))

;; ** eh-website
(use-package org2web
  :commands org2web-publish org2web-new-post)

(use-package eh-website
  :after org2web
  :ensure nil)

(use-package org2web-devtools
  :after org2web
  :ensure nil)

(use-package pyim-devtools
  :after (and org2web pyim)
  :ensure nil)

;; ** el2org
(use-package el2org
  :commands (el2org-generate-readme
             el2org-generate-org
             el2org-generate-html))

;; ** EPG
(use-package epg
  :config
  ;; 1. Put the below to your ~/.gnupg/gpg-agent.conf:
  ;;       allow-emacs-pinentry
  ;;       allow-loopback-pinentry
  ;; 2. gpgconf --reload gpg-agent
  ;; 3. (setq epa-pinentry-mode 'loopback)
  ;; 4. (pinentry-start)
  (setq epa-pinentry-mode 'loopback))

;; ** emms
(use-package eh-emms
  :commands (emms eh-emms emms-browser)
  :ensure nil)

;; ** elisp setting
(use-package lisp-mode
  :mode "\\.el\\'"
  :ensure nil)

(use-package aggressive-indent
  :after lisp-mode
  :config
  (defun eh-elisp-setup ()
    ;; 跟踪行尾空格
    (setq show-trailing-whitespace t)
    ;; 高亮TAB
    (setq highlight-tabs t)
    ;; 自动缩进
    (aggressive-indent-mode))
  (add-hook 'emacs-lisp-mode-hook
            #'eh-elisp-setup))

;; ** ESS
(use-package ess
  :mode "\\.R\\'"
  :commands R
  :ensure nil
  :config
  (setq ess-eval-visibly-p nil)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-smart-S-assign-key "@")

  (defun eh-ess-popup-ESS-buffer (eob-p)
    (interactive "P")
    (ess-force-buffer-current)
    (let ((buffer (current-buffer)))
      (ess-switch-to-ESS eob-p)
      (ess-show-buffer buffer t)))

  (defun eh-ess-eval-paragraph (vis)
    (interactive "P")
    (ess-eval-paragraph-and-step vis)
    (eh-ess-popup-ESS-buffer t))

  :bind (:map
         ess-mode-map
         ("C-c C-c" . eh-ess-eval-paragraph)))

;; ** aggressive-indent
(use-package aggressive-indent)

;; ** multi-term
(use-package multi-term
  :commands multi-term
  :ensure nil
  :config
  (setq multi-term-program "/bin/bash")
  (setq multi-term-buffer-name "term")
  (setq term-scroll-show-maximum-output nil)
  (setq term-scroll-to-bottom-on-output nil)
  (setq multi-term-dedicated-select-after-open-p t)
  (setq term-bind-key-alist
        (append '(("C-c C-x" . eh-term-send-ctrl-x)
                  ("C-c C-h" . eh-term-send-ctrl-h))
                term-bind-key-alist))

  (remove-hook 'term-mode-hook 'eh-term-setup)
  (remove-hook 'term-mode-hook 'multi-term-keystroke-setup)
  (remove-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook)

  (add-hook 'term-mode-hook #'eh-term-setup)
  (add-hook 'term-mode-hook #'multi-term-keystroke-setup)
  (add-hook 'kill-buffer-hook #'multi-term-kill-buffer-hook)

  (defun eh-term-setup ()
    (setq truncate-lines t)
    (setq term-buffer-maximum-size 0)
    (setq show-trailing-whitespace nil)
    (multi-term-handle-close))

  (defun eh-term-send-ctrl-x ()
    "Send C-x in term mode."
    (interactive)
    (term-send-raw-string "\C-x"))

  (defun eh-term-send-ctrl-z ()
    "Send C-z in term mode."
    (interactive)
    (term-send-raw-string "\C-z"))

  (defun eh-term-send-ctrl-h ()
    "Send C-h in term mode."
    (interactive)
    (term-send-raw-string "\C-h")))

;; ** wdired and dired-ranger
(use-package dired
  :commands dired
  :ensure nil)

(use-package wdired
  :after dired
  :ensure nil)

(use-package dired-ranger
  :after dired
  :ensure nil)

;; ** ace-jump
(use-package ace-jump-mode
  :ensure nil
  :bind (("C-j" . ace-jump-mode)))

;; ** gitpatch
(use-package gitpatch
  :bind (("C-c m" . eh-gitpatch-mail))
  :ensure nil
  :config
  (setq gitpatch-mail-function 'gnus-msg-mail)
  (setq gitpatch-mail-attach-patch-key "C-c i")
  (setq gitpatch-mail-database
        '("guix-patches@gnu.org"
          "emms-help@gnu.org"
          "emacs-orgmode@gnu.org"
          "emacs-devel@gnu.org"))
  (defun eh-gitpatch-mail ()
    (interactive)
    ;; 如果 gnus 没有开启，强制开启。
    (let ((buffer (current-buffer)))
      (unless (gnus-alive-p)
        (gnus)
        (switch-to-buffer buffer))
      (call-interactively 'gitpatch-mail))))

;; ** ebdb
(use-package ebdb
  :commands (ebdb ebdb-complete ebdb-complete-enable)
  :ensure nil)

(use-package ebdb-mua
  :after ebdb
  :ensure ebdb)

(use-package ebdb-gnus
  :after ebdb
  :ensure ebdb)

(use-package ebdb-com
  :after ebdb
  :ensure ebdb)

(use-package ebdb-vcard
  :after ebdb
  :ensure ebdb)

(use-package ebdb-complete
  :after ebdb
  :ensure ebdb
  :config
  (ebdb-complete-enable))

(use-package ebdb-i18n-chn
  :after ebdb
  :ensure nil)

(use-package pyim
  :after ebdb-i18n-chn
  :config
  ;; (defun eh-ebdb-search-chinese (string)
  ;;   (if (functionp 'pyim-isearch-build-search-regexp)
  ;;       (pyim-isearch-build-search-regexp string)
  ;;     string))

  ;; (setq ebdb-search-transform-functions
  ;;       '(eh-ebdb-search-chinese))
  (cl-defmethod ebdb-field-search
      :around (field criterion)
      (or (cl-call-next-method)
          (when (stringp criterion)
            (let ((str (ebdb-string field)))
              (cl-some
               (lambda (pinyin)
                 (string-match-p criterion pinyin))
               (append (pyim-hanzi2pinyin str nil "" t)
                       (pyim-hanzi2pinyin str t "" t))))))))

;; ** magit
(use-package magit
  :bind (("C-c g" . magit-status)
         :map magit-status-mode-map
         ("C-c f" . magit-format-patch)))

(use-package swiper
  :after magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package counsel
  :after magit
  :bind (("C-c i" . counsel-git-log))
  :config
  (setq counsel-yank-pop-separator
        (concat "\n\n" (make-string 70 ?-) "\n"))

  (setq counsel-git-log-cmd
        "GIT_PAGER=cat git log --pretty='TUMASHU%%s%%n%%n%%b' --grep '%s'")
  (setq counsel-git-log-split-string-re "TUMASHU"))

;; ** projectile
(use-package projectile
  :bind (("C-x F" . projectile-find-file)
         ("C-S-s" . projectile-grep)))

(use-package swiper
  :after projectile
  :ensure nil
  :config (setq projectile-completion-system 'ivy))

(use-package wgrep
  :after projectile
  :config
  (projectile-global-mode 1)
  (setq projectile-enable-caching nil))

;; ** guix
(use-package guix
  :commands guix-devel-mode
  :ensure nil
  :config
  (setq guix-directory "~/project/guix")
  (setq geiser-debug-jump-to-debug-p nil)
  (setq geiser-guile-binary
        (list (executable-find "guile")
              ;; Avoid auto-compilation as it is slow and error-prone:
              ;; <https://notabug.org/alezost/emacs-guix/issues/2>.
              "--no-auto-compile"))
  (add-hook 'scheme-mode-hook 'guix-devel-mode))

(use-package geiser-guile
  :ensure geiser
  :config
  (add-to-list 'geiser-guile-load-path "~/.config/guix/latest"))

;; ** undo-tree
(use-package undo-tree
  :bind (("C-c /" . undo-tree-visualize))
  :config
  (global-undo-tree-mode)
  (add-hook 'undo-tree-visualizer-mode-hook
            #'eh-undo-tree-visualizer-settings)
  (defun eh-undo-tree-visualizer-settings ()
    (interactive)
    (define-key undo-tree-visualizer-mode-map (kbd "C-c C-k") #'undo-tree-visualizer-quit)
    (define-key undo-tree-visualizer-mode-map (kbd "C-k") #'undo-tree-visualizer-quit)
    (define-key undo-tree-visualizer-mode-map (kbd "k") #'undo-tree-visualizer-quit)
    (define-key undo-tree-visualizer-mode-map (kbd "C-g") #'undo-tree-visualizer-abort)))

;; * Footer
(provide 'eh-misc)

;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:

;;; eh-misc.el ends here
