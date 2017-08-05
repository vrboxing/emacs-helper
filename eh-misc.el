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
;; ** eh-website
(use-package org-webpage
  :config
  ;; my website's owp config
  (use-package eh-website :ensure nil)
  ;; org-webpage's owp config
  (use-package owp-devtools :ensure nil)
  (use-package pyim
    :ensure nil
    :config
    ;; pyim owp config
    (use-package pyim-devtools :ensure nil)))

;; ** el2org
(use-package el2org)

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

;; ** sdcv
(use-package chinese-yasdcv
  :ensure nil
  :if (not (eq system-type 'windows-nt))
  :bind (("C-c d" . yasdcv-translate-at-point)))

;; ** emms
(use-package eh-emms :ensure nil)

;; ** elisp setting
(use-package lisp-mode
  :ensure nil
  :config
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
              #'eh-elisp-setup)))

;; ** org-journal
(use-package org-journal
  :bind (("C-c C-j" . org-journal-new-entry))
  :ensure nil
  :config
  (setq org-journal-dir "E:/doc/journal/")
  (setq org-journal-file-format "%Y%m%d.org")
  (use-package org
    :ensure nil
    :config
    (use-package org-agenda
      :ensure nil
      :config
      (setq org-agenda-files
            (if (file-directory-p org-journal-dir)
                (append (directory-files org-journal-dir t ".org$")
                        org-agenda-files)
              org-agenda-files)))))

;; ** ESS
(use-package ess
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

;; ** autopair
(use-package autopair
  :config
  (autopair-global-mode 1))

;; ** multi-term
(use-package multi-term
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
  :ensure nil
  :config
  (use-package wdired :ensure nil)
  (use-package dired-ranger :ensure nil))

;; ** ace-jump
(use-package ace-jump-mode
  :ensure nil
  :bind (("C-j" . ace-jump-mode)))

;; ** switch-window
(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x 1" . switch-window-then-maximize)
         ("C-x 2" . switch-window-then-split-below)
         ("C-x 3" . switch-window-then-split-right)
         ("C-x 0" . switch-window-then-delete))
  :config
  (setq switch-window-increase 6)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-input-style 'minibuffer))

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
  :ensure nil
  :config
  (require 'ebdb-mua)
  (require 'ebdb-com)
  (require 'ebdb-chn)
  (require 'ebdb-vcard)
  (require 'ebdb-complete)
  (ebdb-complete-enable)

  (use-package pyim
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
                         (pyim-hanzi2pinyin str t "" t)))))))))

;; ** magit
(use-package magit
  :bind (("C-c g" . magit-status)
         :map magit-status-mode-map
         ("C-c f" . magit-format-patch))
  :config
  (use-package swiper
    :config
    (setq magit-completing-read-function 'ivy-completing-read)))

;; ** projectile
(use-package projectile
  :bind (("C-x F" . projectile-find-file)
         ("C-S-s" . projectile-grep))
  :config
  (use-package swiper
    :ensure nil
    :config (setq projectile-completion-system 'ivy))
  (use-package wgrep
    :config
    (projectile-global-mode 1)
    (setq projectile-enable-caching nil)))

;; ** guix
(use-package guix
  :ensure nil
  :config
  (setq guix-directory "~/project/guix")
  (setq geiser-debug-jump-to-debug-p nil)
  (add-hook 'after-init-hook 'global-guix-prettify-mode)
  (add-hook 'scheme-mode-hook 'guix-devel-mode))

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

;; ** emacs-calfw
(use-package holidays
  :ensure nil
  :config
  (defvar eh-calendar-holidays nil)
  (setq eh-calendar-holidays
        '(;;公历节日
          (holiday-fixed 1 1 "元旦")
          (holiday-fixed 2 14 "情人节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 3 14 "白色情人节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 5 1 "劳动节")
          (holiday-fixed 5 4 "青年节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-fixed 9 10 "教师节")
          (holiday-fixed 10 1 "国庆节")
          (holiday-fixed 12 25 "圣诞节")
          ;; 农历节日
          (holiday-lunar 1 1 "春节" 0)
          (holiday-lunar 1 2 "春节" 0)
          (holiday-lunar 1 3 "春节" 0)
          (holiday-lunar 1 15 "元宵节" 0)
          (holiday-solar-term "清明" "清明节")
          (holiday-solar-term "小寒" "小寒")
          (holiday-solar-term "大寒" "大寒")
          (holiday-solar-term "立春" "立春")
          (holiday-solar-term "雨水" "雨水")
          (holiday-solar-term "惊蛰" "惊蛰")
          (holiday-solar-term "春分" "春分")
          (holiday-solar-term "谷雨" "谷雨")
          (holiday-solar-term "立夏" "立夏")
          (holiday-solar-term "小满" "小满")
          (holiday-solar-term "芒种" "芒种")
          (holiday-solar-term "夏至" "夏至")
          (holiday-solar-term "小暑" "小暑")
          (holiday-solar-term "大暑" "大暑")
          (holiday-solar-term "立秋" "立秋")
          (holiday-solar-term "处暑" "处暑")
          (holiday-solar-term "白露" "白露")
          (holiday-solar-term "秋分" "秋分")
          (holiday-solar-term "寒露" "寒露")
          (holiday-solar-term "霜降" "霜降")
          (holiday-solar-term "立冬" "立冬")
          (holiday-solar-term "小雪" "小雪")
          (holiday-solar-term "大雪" "大雪")
          (holiday-solar-term "冬至" "冬至")
          (holiday-lunar 5 5 "端午节" 0)
          (holiday-lunar 8 15 "中秋节" 0)
          (holiday-lunar 7 7 "七夕情人节" 0)
          (holiday-lunar 12 8 "腊八节" 0)
          (holiday-lunar 9 9 "重阳节" 0)
          (holiday-lunar 12 22 "冬至" 0)))
  (setq calendar-holidays eh-calendar-holidays))

(use-package calendar
  :ensure nil
  :config
  (setq calendar-month-name-array
        ["一月" "二月" "三月" "四月" "五月" "六月"
         "七月" "八月" "九月" "十月" "十一月" "十二月"])
  (setq calendar-day-name-array
        ["星期日" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六"])

  ;; 一周第一天，0表示星期天, 1表示星期一
  (setq calendar-week-start-day 0))

(use-package calfw
  :ensure nil
  :config
  (use-package cal-china-x :ensure nil)

  (defvar eh-calfw-org-file nil)
  (setq eh-calfw-org-file "~/org/calfw.org")

  ;; 日历表格边框设置
  (setq cfw:fchar-junction ?+
        cfw:fchar-vertical-line ?|
        cfw:fchar-horizontal-line ?-
        cfw:fchar-left-junction ?+
        cfw:fchar-right-junction ?+
        cfw:fchar-top-junction ?+
        cfw:fchar-top-left-corner ?+
        cfw:fchar-top-right-corner ?+)

  (setq cfw:gettext-alist
        '(("Today" . "t:今天")
          ("Month" . "M:一月")
          ("Week" . "W:一周")
          ("Two Weeks" . "T:两周")
          ("Day" . "D:一天")))

  (use-package calfw-org
    :ensure nil
    :config
    (defun eh-calendar ()
      (interactive)
      (cfw:open-calendar-buffer
       :view 'two-weeks
       :contents-sources
       (list
        ;; orgmode source
        (cfw:org-create-source "Green")))))

  (use-package org-agenda
    :ensure nil
    :config
    (unless (member eh-calfw-org-file org-agenda-files)
      (push eh-calfw-org-file org-agenda-files))
    (use-package org-capture
      :ensure nil
      :config
      ;; 为calfw设置一个capture模板并添加到org-capture-templates
      (setq cfw:org-capture-template
            '("calfw2org" "calfw2org" entry (file+headline eh-calfw-org-file "Schedule")
              "* %?\n %(cfw:org-capture-day)\n %a"))
      (setq org-capture-templates
            (append org-capture-templates (list cfw:org-capture-template))))))

;; * Footer
(provide 'eh-misc)

;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:

;;; eh-misc.el ends here
