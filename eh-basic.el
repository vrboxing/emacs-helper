;;; eh-basic.el --- Tumashu's basic emacs configuation

;; * Header
;; Copyright (c) 2011-2016, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.3

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

;; * 代码                                                                 :code:
(require 'cl-lib)
(require 'eh-functions)

;; ** 设置 load-path
(defun eh-hack-load-path ()
  ;; Delete buildin org's PATH
  (setq load-path
        (cl-remove-if
         #'(lambda (path)
             (string-match "lisp/org$" path))
         load-path))
  ;; Demove property lists to defeat cus-load and remove autoloads
  (mapatoms
   #'(lambda (sym)
       (let ((sym-name (symbol-name sym)))
         (when (string-match "^\\(org\\|ob\\|ox\\)-?" sym-name)
           (setplist sym nil)
           (when (autoloadp sym)
             (unintern sym)))))))

(defun eh-update-load-path ()
  (interactive)
  (let (dirs)
    (dolist (x '("~" "c:" "d:" "e:" "f:" "g:" "h:" "i:"))
      (push (file-name-as-directory
             (concat x "/projects/emacs-packages")) dirs)
      (push (file-name-as-directory
             (concat x "/project/emacs-packages")) dirs))
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (x (directory-files dir t))
          (when (and (file-directory-p x)
                     (not (string-match-p "/\\.$" x))
                     (not (string-match-p "/\\.\\.$" x)))
            (add-to-list 'load-path x))))))
  (eh-hack-load-path))

(eh-update-load-path)

;; ** Full name and Email
(setq user-full-name "Feng Shu")
(setq user-mail-address "tumashu@163.com")

;; ** 启动时默认打开的 buffer.
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq initial-scratch-message
      ";; This is *scratch* buffer.\n\n")

;; ** 使用空格缩进
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)

;; ** 关闭 beep
(setq visible-bell t)

;; ** 让 *scratch* buffer 无法删除
(defun eh-unkillable-scratch-buffer ()
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))

(add-hook 'kill-buffer-query-functions
          #'eh-unkillable-scratch-buffer)

;; ** 使用 use-package
(require 'use-package)

;; ** 设置 emacs 包管理器
(use-package package
  :config
  (package-initialize)

  (defun eh-elpa-directory ()
    "返回 emacs-helper 内置 elpa 镜像的目录。"
    (file-name-as-directory
     (concat
      (file-name-directory
       (locate-library "eh-basic.el"))
      "elpa/")))

  (setq package-archives
        `(("eh-elpa" . ,(eh-elpa-directory))
          ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
          ("org-cn"   . "http://elpa.emacs-china.org/org/")
          ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/"))))

;; ** 设置主题
(use-package cyberpunk-theme
  :config
  (add-hook 'after-init-hook
            #'(lambda ()
                (load-theme 'cyberpunk t))))
;; ** 启用 async
(use-package async)

;; ** 设置 elpa-mirror
(use-package elpa-mirror
  :config

  (defun eh-elpa-mirror-github ()
    (interactive)
    (let ((directory "~/.eh-elpa-mirror/"))
      (elpamr-create-mirror-for-installed directory t)
      (shell-command
       (concat "cd " directory " && "
               "git init &&"
               "git add -A && "
               "git commit -m \"Update elpa mirror\" && "
               "git push -f git@github.com:tumashu/elpa.git master &"))))

  (defun eh-elpa-mirror ()
    (interactive)
    (let* ((directory (file-name-as-directory (eh-elpa-directory)))
           (recreate-directory
            (yes-or-no-p (format "重新创建目录：%S ? " directory))))
      (elpamr-create-mirror-for-installed directory recreate-directory))))

;; ** 设置 Charset
(use-package mule
  :ensure nil
  :config

  (set-language-environment "UTF-8")
  (set-buffer-file-coding-system 'utf-8-unix)
  (set-clipboard-coding-system 'utf-8-unix)
  (set-file-name-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-next-selection-coding-system 'utf-8-unix)
  (set-selection-coding-system 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)

  (when (eq system-type 'windows-nt)
    (set-language-environment "Chinese-GBK")
    (set-selection-coding-system 'gbk-dos)
    (set-next-selection-coding-system 'gbk-dos)
    (set-clipboard-coding-system 'gbk-dos)))

;; ** 保存文件之前，删除无用的空格
(use-package files
  :ensure nil
  :config
  ;; 使用下面这一行配置后，org-mode 的源代码总是莫名其妙的
  ;;     (add-hook 'before-save-hook #'whitespace-cleanup)
  ;; 更改，这会导致生成的 diff 相当乱。
  (use-package whitespace
    :ensure nil)
  (use-package simple
    :ensure nil
    :config
    (add-hook 'before-save-hook
              #'(lambda ()
                  (delete-trailing-whitespace)))))

;; ** 设置 Eshell
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

;; ** 设置 EWW
(use-package eww
  :ensure nil
  :config
  (setq shr-width 90)
  ;; 搜狗:  http://www.sogou.com/sogou?query=
  ;; 百度:  http://m.baidu.com/ssid=0/s?word=
  ;; 必应:  http://cn.bing.com/search?q=
  (setq eww-search-prefix "http://www.sogou.com/sogou?query="))

;; ** 设置拼音输入法
(use-package chinese-pyim
  :ensure nil
  :config
  ;; 激活 basedict 词库
  (use-package chinese-pyim-basedict
    :ensure nil
    :config (chinese-pyim-basedict-enable))

  (setq default-input-method "chinese-pyim")

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
  (setq pyim-isearch-enable-pinyin-search t)

  ;; 使用 pupup 来绘制选词框。
  (setq pyim-page-tooltip 'popup)

  ;; 显示5个候选词。
  (setq pyim-page-length 5)

  ;; emacs 启动时加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda ()
                (pyim-restart-1 t)))
  :bind
  (("M-j" . pyim-convert-code-at-point)
   ("C-;" . pyim-delete-word-from-personal-buffer)))

;; ** 设置中文字体
(use-package chinese-fonts-setup
  :demand t
  :init (setq cnfonts-verbose nil)
  :config
  (setq cnfonts-use-face-font-rescale
        (eq system-type 'gnu/linux))
  (cnfonts-enable)
  :bind (("C--" . cnfonts-decrease-fontsize)
         ("C-=" . cnfonts-increase-fontsize)
         ("C-+" . cnfonts-next-profile)))

;; ** 设置 recentf
(use-package recentf
  :ensure nil
  :bind (("C-x f" . recentf-open-files))
  :config
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1)
  (setq recentf-max-saved-items 99)
  (setq recentf-max-menu-items 99)
  (setq recentf-show-file-shortcuts-flag nil)
  (setq recentf-exclude
        '("COMMIT" "autoloads" "archive-contents" "eld" "newsrc"
          ".recentf" "emacs-font-size.conf"
          "pyim-dcache-.*"))
  ;; 自动保存recentf文件。
  (add-hook 'find-file-hook #'recentf-save-list))

;; ** 设置 ibuffer
(use-package ibuffer
  :ensure nil
  :bind (("C-x b" . ibuffer)))

;; ** 设置区域选择快捷键
(use-package simple
  :ensure nil
  :init (global-unset-key (kbd "C-x C-x"))
  :bind
  (("C-x <SPC>" . set-mark-command)
   ;; QQ 将会在 emacs 之前捕捉 M-w 快捷键，记得取消。
   ;; 另外绑定 C-c w 作为备用。
   ("C-c w" . kill-ring-save)
   ("C-x C-x C-x" . exchange-point-and-mark)))

(use-package rect
  :ensure nil
  :bind (("C-x C-x <SPC>" . rectangle-mark-mode)))

;; ** 关闭 tool-bar
(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1)
  :bind (("C-x k" . kill-this-buffer)))

;; ** 关闭 menu-bar
(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode 0)
  :bind (("C-x k" . kill-this-buffer)))

;; ** 关闭 scroll-bar
(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

;; ** 配对括号高亮显示
(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1))

;; * Footer
(provide 'eh-basic)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-basic.el ends here
