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
(defvar eh-directory
  (file-name-directory
   (locate-library "eh-basic.el"))
  "eh-basic.el 文件所在的目录。")

(defvar eh-elpa-directory
  (file-name-as-directory
   (concat eh-directory "elpa/"))
  "emacs-helper 内置 elpa 镜像的目录。")

;; Full name and email
(setq user-full-name "Feng Shu")
(setq user-mail-address "tumashu@163.com")

;; Startup screen
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq initial-scratch-message ";; This is *scratch* buffer.\n\n")

;; 使用空格缩进
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)

;; 关闭 beep
(setq visible-bell t)

;; Don't delete *scratch* buffer
(defun eh-unkillable-scratch-buffer ()
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))

(add-hook 'kill-buffer-query-functions 'eh-unkillable-scratch-buffer)

;; load-path
(defvar eh-enable-load-path-hack t)
(defvar eh-enable-load-path-update nil)

(defun eh-hack-load-path ()
  ;; Delete buildin org's PATH
  (setq load-path
        (delq nil (mapcar
                   #'(lambda (p)
                       (unless (or (string-match "lisp/org$" p))
                         p))
                   load-path)))
  ;; Demove property lists to defeat cus-load and remove autoloads
  (mapatoms #'(lambda (s)
                (let ((sn (symbol-name s)))
                  (when (string-match "^\\(org\\|ob\\|ox\\)-?" sn)
                    (setplist s nil)
                    (when (autoloadp s)
                      (unintern s)))))))

(defun eh-update-load-path ()
  (interactive)
  (let (dirs)
    (dolist (x '("~" "c:" "d:" "e:" "f:" "g:"))
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
  (when eh-enable-load-path-hack
    (eh-hack-load-path)))

(when eh-enable-load-path-update
  (eh-update-load-path)
  (message "emacs-helper update load-path success!"))

;; package
(require 'package)
(setq package-archives `(("eh-elpa" . ,eh-elpa-directory)))
(package-initialize)

(defun eh-packages-install (packages)
  (let ((refreshed nil))
    (when (not package-archive-contents)
      (package-refresh-contents)
      (setq refreshed t))
    (dolist (pkg packages)
      (when (and (not (package-installed-p pkg))
                 (assoc pkg package-archive-contents))
        (unless refreshed
          (package-refresh-contents)
          (setq refreshed t))
        (package-install pkg)))))

(defun eh-update-package-archives ()
  (interactive)
  (setq package-archives
        `(("eh-elpa" . ,eh-elpa-directory)
          ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
          ("org-cn"   . "http://elpa.emacs-china.org/org/")
          ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/"))))

;; 安装 emacs-helper/elpa 目录下的所有的包
(defun eh-get-elpa-mirror-packages ()
  (let ((file (concat (file-name-directory eh-elpa-directory)
                      "archive-contents")))
    (when (file-exists-p file)
      (mapcar #'car (cdr (read
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))))))))

(defvar eh-enable-full-install nil)

(if eh-enable-full-install
    (eh-packages-install (eh-get-elpa-mirror-packages))
  (eh-packages-install '(use-package org org-plus-contrib)))

;; use-package
(require 'use-package)

;; 默认不显示 *Async Shell Command* buffer
;; (add-to-list 'display-buffer-alist
;;              '("\\*Async Shell Command\\*.*"  display-buffer-no-window nil))

;; Theme 设置
(use-package cyberpunk-theme
  :config
  (add-hook 'after-init-hook
            #'(lambda ()
                (load-theme 'cyberpunk t))))
;; async
(use-package async)

;; elpa-mirror 设置
(use-package elpa-mirror
  :config
  (defun eh-update-elpa ()
    (interactive)
    (let* ((dir (file-name-as-directory eh-elpa-directory))
           (elpamr-default-output-directory dir))
      (when (y-or-n-p (format "更新 emacs/elpa 目录：%S ? " dir))
        (when (file-directory-p dir)
          (delete-directory dir t))
        (make-directory dir t))
      (elpamr-create-mirror-for-installed))))

;; Charset 设置
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

;; 保存文件之前，删除无用的空格
(use-package files
  :ensure nil
  :config
  (use-package whitespace
    :ensure nil
    :config)
    ;; 开启这个 hook 后，org-mode 的源代码总是莫名其妙的
    ;; 被更改，导致 diff 相当乱，暂时禁用吧。
    ;; (add-hook 'before-save-hook 'whitespace-cleanup)

  (use-package simple
    :ensure nil
    :config
    (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))))

;; eshell
(use-package eshell
  :bind (("C-x c" . eshell))
  :ensure nil
  :config

  (use-package em-term
    :ensure nil)
  (use-package em-unix
    :ensure nil)

  (setq eshell-visual-commands
        (append '("aptitude" "mutt" "nano" "crontab" "vim" "less")
                eshell-visual-commands))
  (setq eshell-visual-subcommands
        (list (append '("sudo") eshell-visual-commands)
              '("git" "log" "diff" "show")))
  (setq eshell-visual-options
        '(("git" "--help")))

  (defun eh-eshell (&optional arg)
    (interactive)
    ;; 使用eshell-exec-visual第一次打开term时，
    ;; 不能使用multi-term的键盘绑定，原因不知，
    ;; 首先运行一下less, 从而让multi-term的键盘绑定生效。
    (eshell-command "less")
    (eshell arg)))

;; (setenv "PATH"
;;         (concat "/usr/local/texlive/2014/bin/i386-linux:"
;;                 (getenv "PATH")))
;; (setenv "PYTHONPATH"
;;         (concat "~/project/emacs-packages/emacs-helper/doc/configs:"
;;                 (getenv "PYTHONPATH")))
;; (setq exec-path
;;       (append '("/usr/local/texlive/2014/bin/i386-linux")
;;               exec-path))

;; eww
(use-package eww
  :ensure nil
  :config
  (setq shr-width 90)
  ;; 搜狗:  http://www.sogou.com/sogou?query=
  ;; 百度:  http://m.baidu.com/ssid=0/s?word=
  ;; 必应:  http://cn.bing.com/search?q=
  (setq eww-search-prefix "http://www.sogou.com/sogou?query="))

;; Pinyin Input Method
(use-package chinese-pyim
  :ensure nil
  :config
  ;; 激活 basedict 词库
  (use-package chinese-pyim-basedict
    :ensure nil
    :config (chinese-pyim-basedict-enable))

  (use-package chinese-pyim-wbdict
    :ensure nil
    :config (chinese-pyim-wbdict-gbk-enable))

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
            #'(lambda () (pyim-restart-1 t)))
  :bind
  (("M-j" . pyim-convert-code-at-point)
   ("C-;" . pyim-delete-word-from-personal-buffer)))

;; Chinese fonts setup
(use-package chinese-fonts-setup
  :demand t
  :init (setq cfs-verbose nil)
  :config
  (setq cfs-use-face-font-rescale (eq system-type 'gnu/linux))
  (chinese-fonts-setup-enable)
  :bind (("C--" . cfs-decrease-fontsize)
         ("C-=" . cfs-increase-fontsize)
         ("C-+" . cfs-next-profile)))

;; recentf
(use-package recentf
  :ensure nil
  :bind (("C-x f" . recentf-open-files))
  :config
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1)
  (setq recentf-max-saved-items 99)
  (setq recentf-max-menu-items 99)
  (setq recentf-exclude
        '("COMMIT" "autoloads" "archive-contents" "eld" "newsrc"
          ".recentf" "emacs-font-size.conf"
          "pyim-dcache-.*"))
  (setq recentf-menu-filter 'eh-recentf-buffer-filter)
  (setq recentf-show-file-shortcuts-flag nil)

  (defun eh-recentf-buffer-filter (l)
    (let ((index 0)
          filtered-list element list name recentf-string)
      (dolist (elt l (nreverse filtered-list))
        (setq index (1+ index)
              element (recentf-menu-element-value elt)
              list (reverse (split-string element "/"))
              name (if (> (length (nth 0 list)) 0)
                       (format "%s" (nth 0 list))
                     (format "%s/" (nth 1 list)))
              recentf-string (format "[%2s]:  %-30s (%s)" index name element))
        (push (recentf-make-menu-element recentf-string element) filtered-list))))

  ;; 自动保存recentf文件。
  (add-hook 'find-file-hook 'recentf-save-list))

;; ibuffer
(use-package ibuffer
  :ensure nil
  :bind (("C-x b" . ibuffer)))

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

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1)
  :bind (("C-x k" . kill-this-buffer)))

(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode 0)
  :bind (("C-x k" . kill-this-buffer)))

(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

(use-package paren
  :ensure nil
  :config
  ;; 高亮配对的括号
  (show-paren-mode 1))

;; * Footer
(provide 'eh-basic)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-basic.el ends here
