;;; eh-complete.el --- Tumashu's emacs complete configuation

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
;; smex swiper and ivy-mode
(use-package smex
  :config (smex-initialize))

(use-package swiper
  :config
  (use-package counsel
    :config
    (setq counsel-yank-pop-separator
          (concat "\n\n" (make-string 70 ?-) "\n"))
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
        ;; ivy-count-format "%-2d "
        ivy-use-virtual-buffers t
        ivy-format-function 'ivy-format-function-arrow
        ivy-display-style 'fancy)
  (push '(counsel-M-x . "") ivy-initial-inputs-alist)
  (push '(counsel-describe-function . "") ivy-initial-inputs-alist)
  (push '(counsel-describe-variable . "") ivy-initial-inputs-alist)
  ;; I use "C-x C-f" to open file, so bind "C-f" to
  ;; `ivy-immediate-done' is very useful.
  (define-key ivy-minibuffer-map (kbd "C-f") 'ivy-immediate-done))

;; company-mode
(use-package company
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-echo-delay 0)
  (setq company-global-modes
        '(not message-mode git-commit-mode eshell-mode))

  ;; company-dabbrev
  (setq company-dabbrev-char-regexp "[[:word:]_:@.-]+")
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-require-match nil)
  (setq company-dabbrev-minimum-length 2)

  (setq company-backends
        '((company-capf company-dabbrev company-files)
          (company-dabbrev-code company-gtags company-etags
                                company-keywords)))
  (setq company-transformers
        '(company-sort-by-occurrence))

  (setq company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))

  (global-set-key (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-i") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p")'company-select-previous)
  (define-key company-active-map (kbd "M-n") 'company-select-next)
  (define-key company-active-map (kbd "M-p")'company-select-previous)

  (if (and (fboundp 'daemonp) (daemonp))
      (add-hook 'after-make-frame-functions
                (lambda (x)
                  (global-company-mode)))
    (global-company-mode))

  (defun eh-company-dabbrev--prefix (orig-fun)
    "取消中文补全"
    (let ((string (pyim-char-before-to-string 0)))
      (if (pyim-string-match-p "\\cc" string)
          nil
        (funcall orig-fun))))

  (advice-add 'company-dabbrev--prefix :around #'eh-company-dabbrev--prefix))

;; * Footer
(provide 'eh-complete)

;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:

;;; eh-complete.el ends here
