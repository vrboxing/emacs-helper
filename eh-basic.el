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

;; ** 设置 load-path (Can not use use-package)
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
    (dolist (x '("~" "c:" "d:" "e:" "f:" "g:" "h:" "i:" "~/storage/shared/"))
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

;; ** Full name and Email (Can not use use-package)
(setq user-full-name "Feng Shu")
(setq user-mail-address "tumashu@163.com")

;; ** 启动时默认打开的 buffer. (Can not use use-package)
(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq initial-scratch-message
      ";; This is *scratch* buffer.\n\n")

;; ** 使用空格缩进 (Can not use use-package)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)

;; ** 关闭 beep (Can not use use-package)
(setq visible-bell t)

;; ** 让 *scratch* buffer 无法删除 (Can not use use-package)
(defun eh-unkillable-scratch-buffer ()
  (if (string= (buffer-name (current-buffer)) "*scratch*")
      (progn
        (delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))

(add-hook 'kill-buffer-query-functions
          #'eh-unkillable-scratch-buffer)

;; ** 设置 emacs 包管理器 (Can not use use-package)
(require 'package)
(package-initialize)

(defun eh-elpa-directory ()
  "返回 emacs-helper 内置 elpa 镜像的目录。"
  (file-name-as-directory
   (concat (file-name-directory
            (locate-library "eh-basic.el"))
           "elpa/")))

(setq package-archives
      `(("eh-elpa" . ,(eh-elpa-directory))
        ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
        ("org-cn"   . "http://elpa.emacs-china.org/org/")
        ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))

;; ** 使用 use-package
(require 'use-package)

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

  ;; Window 系统下关闭 Emacs 时，强制确认，防止误操作。
  (when (eq system-type 'windows-nt)
    (setq confirm-kill-emacs 'yes-or-no-p))

  ;; ** 关闭自动备份功能，我有 git :-)
  (setq make-backup-files nil)

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

;; ** 括号自动匹配
(use-package autopair
  :config
  (autopair-global-mode 1))

;; ** switch-window
(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x 1" . switch-window-then-maximize)
         ("C-x 2" . switch-window-then-split-below)
         ("C-x 3" . switch-window-then-split-right)
         ("C-x 0" . switch-window-then-delete))
  :config
  (unless (display-graphic-p)
    (setq switch-window-shortcut-appearance 'asciiart))
  (setq switch-window-increase 6)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-input-style 'minibuffer))

;; ** 设置 smex
(use-package smex
  :config
  (smex-initialize))

;; ** 设置 swiper 和 ivy-mode
(use-package swiper
  :config

  (use-package counsel
    :bind
    (("C-c C-r" . ivy-resume)
     ("M-x" . counsel-M-x)
     ("C-x b" . ivy-switch-buffer)
     ("C-x f" . counsel-recentf)
     ("C-x C-b" . ivy-switch-buffer)
     ("C-x C-f" . counsel-find-file)
     ("C-h f" . counsel-describe-function)
     ("C-h v" . counsel-describe-variable)
     ("C-c y" . counsel-yank-pop)))

  (ivy-mode 1)
  (setq ivy-count-format ""
        ivy-use-virtual-buffers t
        ivy-format-function #'ivy-format-function-arrow
        ivy-display-style 'fancy)
  (push '(counsel-M-x . "") ivy-initial-inputs-alist)
  (push '(counsel-describe-function . "") ivy-initial-inputs-alist)
  (push '(counsel-describe-variable . "") ivy-initial-inputs-alist)
  ;; I use "C-x C-f" to open file, so bind "C-f" to
  ;; `ivy-immediate-done' is very useful.
  (define-key ivy-minibuffer-map (kbd "C-f") #'ivy-immediate-done))

;; * Footer
(provide 'eh-basic)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-basic.el ends here
