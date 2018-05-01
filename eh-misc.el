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
                (set-face-attribute 'org-agenda-date-today nil :slant 'normal)
                (eval-after-load 'org-column
                  (set-face-attribute 'org-column nil :background "#4D4D4D")
                  (set-face-attribute 'org-column-title nil :background "#4D4D4D"))
                )))

;; ibuffer
(use-package ibuffer
  :ensure nil
  :config
  ;; Make ibuffer work well with termux
  (define-key ibuffer-name-map [(mouse-2)] 'ibuffer-mouse-toggle-mark)
  (define-key ibuffer-name-map [(mouse-1)] 'ibuffer-mouse-visit-buffer))

;; ** VC
(use-package vc
  :ensure nil
  :config
  ;; I use magit instead of vc :-)
  (setq vc-handled-backends nil)
  (setq vc-ignore-dir-regexp ".*"))

;; ** Tramp (msys2's emacs) 和 termux 的 sshd 配合使用需要如下设置：
;;
;; *** 基本设置
;; 1. 将 "/data/data/com.termux/files/usr/bin" 添加到 tramp-remote-path
;; 2. 清理 termux 文件：/data/data/com.termux/files/usr/etc/motd ,
;;    去掉文件中的所有的 "<" 和 ">",因为这两个字符会影响 tramp 登录，
;;    具体细节见 Tramp FAQ： https://www.gnu.org/software/tramp/
;;
;; *** 使用 ssh 相关方法的设置
;; 使用 ssh 的相关 tramp 方法有： ssh, sshx, scp, scpx
;;
;; 1. ssh 和 scp 两种 tramp 方法会让 emacs 卡死，原因可能和 cygwin 遇到
;;    的情况类似：
;;
;;    #+BEGIN_EXAMPLE
;;    Pseudo-terminal will not be allocated because stdin is not a terminal.
;;    #+END_EXAMPLE
;;
;;    具体请参考 Tramp 文档的相关章节： "Issues with Cygwin ssh"
;;    https://www.gnu.org/software/tramp/
;;
;; 2. sshx 和 scpx 可以正常使用，大文件访问使用 scpx 方法速度比较快。
;; 3. 建议使用 key + ssh-agent 的方式登录，具体设置方法请自行搜索。
;;
;;    下面是一个启动 ssh-agent 的脚本，调整以下贴到 ~/.bashrc 文件就可以了。
;;
;;    #+BEGIN_SRC shell
;;    if [ -f ~/.agent.env ]; then
;;        . ~/.agent.env >/dev/null
;;        if ! kill -0 $SSH_AGENT_PID >/dev/null 2>&1; then
;;            echo "Stale agent file found. Spawning new agent..."
;;            eval `ssh-agent |tee ~/.agent.env`
;;            ssh-add
;;        fi
;;    else
;;        echo "Starting ssh-agent..."
;;        eval `ssh-agent |tee ~/.agent.env`
;;        ssh-add
;;    fi
;;    #+END_SRC
;;
;; *** 使用 Putty 相关方法的设置
;; 使用 putty 的 tramp 方法有四种： plink, plinks, pscp 和 psftp
;;
;; putty 在 window 平台下有图形界面，使用起来很方便，推荐使用，
;; 但 putty, msys2 和 termux 三者配合需要做一些配置。
;;
;; 1. 安装 putty, plink 和 ssh-pageant 三个外部程序：
;;
;;    #+BEGIN_EXAMPLE
;;    pacman -Ss mingw-w64-i686-putty mingw-w64-i686-putty-ssh ssh-pageant-git
;;    #+END_EXAMPLE
;;
;; 2. 在 msys2 的 .bashrc 中设置环境变量: MSYS2_ARG_CONV_EXCL, 比如：
;;
;;    #+BEGIN_EXAMPLE
;;    export MSYS2_ARG_CONV_EXCL="192.168.137.250:;feng@192.168.137.250:"
;;    #+END_EXAMPLE
;;
;;    这个设置的意思是：在调用命令时，所有以 "192.168.137.250:" 或者
;;    "feng@192.168.137.250:" 开头的命令参数，都保持原样，不要作 msys2
;;    文件路径转换，比如：
;;
;;    #+BEGIN_EXAMPLE
;;    pscp feng@192.168.137.250:/test.org ~/test.org
;;    #+END_EXAMPLE
;;
;;    这个命令中 "feng@192.168.137.250:/test.org" 这个参数实例将被保护，
;;    不作转换。
;;
;;    这个步骤对于 pscp 和 psftp 两个 tramp 方法非常重要， 不然 pscp 会报错：
;;    "ssh_init Host does not exit"
;;
;;    想了解具体细节同学可以阅读：
;;    1. https://github.com/msys2/msys2/wiki/Porting#user-content-filesystem-namespaces
;;    2. https://stackoverflow.com/questions/41789559/how-to-prevent-msys-from-converting-remote-file-path-for-pscp
;;
;; 3. 设置 putty, 最好设置为免密码登录，比如：key + ssh-pageant 的方式。
;;    具体方法请自行搜索。
;; 4. 将 "C:\msys32\mingw32\bin\pageant.exe" 的快捷方式添加到启动菜单。
;;    更改快捷方式的目标：
;;
;;    #+BEGIN_EXAMPLE
;;    C:\msys32\mingw32\bin\pageant.exe YOUR-PUTTY-KEY-PATH
;;    #+END_EXAMPLE
;;
;; 5. 如果用户使用 plinkx 方法, 还需要设置保存一个 putty session, 这个
;;    putty session 的名字与 host 的名字一致， 比如用 tramp 访问：
;;
;;    #+BEGIN_EXAMPLE
;;    /plinkx:192.168.1.101:/
;;    #+END_EXAMPLE
;;
;;    就需要设置保存一个名字为 "192.168.1.101" 的 putty session .
(use-package tramp
  :ensure nil
  :config
  (push "/data/data/com.termux/files/usr/bin" tramp-remote-path))

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
  :bind (("M-j" . pyim-convert-code-at-point)
         ("C-;" . pyim-delete-word-from-personal-buffer))

  :config

  (when (eh-termux-p)
    ;; 在 Termux 上 emacs-async 暂时无法正常工作，所以没法
    ;; 自动更新 dcache.
    (setq pyim-dcache-auto-update nil))

  (setq default-input-method "pyim")

  ;; 使用 emacs thread 来生成 dcache。
  (setq pyim-dcache-prefer-emacs-thread t)

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

  ;; 设置选词框的绘制方式
  (if (and (display-graphic-p)
           (>= emacs-major-version 26))
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))

  ;; 显示5个候选词。
  (setq pyim-page-length 5)

  ;; emacs 启动时加载 pyim 词库
  ;; (add-hook 'emacs-startup-hook
  ;;           #'(lambda ()
  ;;               (pyim-restart-1 t)))

  )

(use-package pyim-basedict
  :after pyim
  :config (pyim-basedict-enable))

(use-package pyim
  :after ivy
  :config

  (defun eh-ivy-cregexp (str)
    (if (string-match-p "^\\." str)
        (pyim-cregexp-build (substring str 1))
      (ivy--regex-plus str)))

  (setq ivy-re-builders-alist
        '((t . eh-ivy-cregexp))))

;; ** cnfonts
(use-package cnfonts
  :demand t
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
  :commands (org2web-publish org2web-new-post))

(use-package eh-website
  :after org2web
  :ensure nil)

(use-package org2web-devtools
  :after org2web
  :ensure nil)

(use-package pyim-devtools
  :after (:all org2web pyim)
  :ensure nil)

;; ** el2org
(use-package el2org
  :commands (el2org-generate-readme
             el2org-generate-org
             el2org-generate-html))

;; ** EPG
(use-package epg
  :after gnus  ;; Only use it in gnus
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
(use-package elisp-mode
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :ensure nil)

(use-package aggressive-indent
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
  :commands (guix-scheme-mode guix-devel-mode)
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
  :commands run-guile
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
