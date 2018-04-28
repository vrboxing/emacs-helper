;;; eh-termux.el --- Tumashu's emacs termux configuation

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

(defun eh-termux-default-mode-line ()
  (list (propertize
         "[M-x]"
         'mouse-face 'mode-line-highlight
         'keymap
         (let ((map (make-sparse-keymap)))
           (define-key map [mode-line mouse-1] 'counsel-M-x)
           map))
        " "
        (propertize
         "[C-g]"
         'mouse-face 'mode-line-highlight
         'keymap
         (let ((map (make-sparse-keymap)))
           (define-key map [mode-line mouse-1]
             #'(lambda ()
                 (interactive)
                 (execute-kbd-macro (kbd "C-g"))))
           map))
        " "
        (propertize
         "[切]"
         'mouse-face 'mode-line-highlight
         'keymap
         (let ((map (make-sparse-keymap)))
           (define-key map [mode-line mouse-1] 'ibuffer)
           map))
        " "
        (propertize
         "[大]"
         'mouse-face 'mode-line-highlight
         'keymap
         (let ((map (make-sparse-keymap)))
           (define-key map [mode-line mouse-1] 'delete-other-windows)
           map))
        " "
        (when (and (buffer-file-name)
                   (buffer-modified-p))
          (list (propertize
                 "[存]"
                 'mouse-face 'mode-line-highlight
                 'keymap
                 (let ((map (make-sparse-keymap)))
                   (define-key map [mode-line mouse-1] 'save-buffer)
                   map))
                " "))
        (when (eq major-mode 'org-mode)
          (list (propertize
                 "[C-c C-c]"
                 'mouse-face 'mode-line-highlight
                 'keymap
                 (let ((map (make-sparse-keymap)))
                   (define-key map [mode-line mouse-1] 'org-ctrl-c-ctrl-c)
                   map))
                " "))
        "%b"))

(defun eh-termux-capture-mode-line ()
  (list "Capture: "
        (propertize
         "[完成]"
         'mouse-face 'mode-line-highlight
         'keymap
         (let ((map (make-sparse-keymap)))
           (define-key map [mode-line mouse-1] 'org-capture-finalize)
           map))
        " "
        (propertize
         "[取消]"
         'mouse-face 'mode-line-highlight
         'keymap
         (let ((map (make-sparse-keymap)))
           (define-key map [mode-line mouse-1] 'org-capture-kill)
           map))
        " "
        (propertize
         "[Refile]"
         'mouse-face 'mode-line-highlight
         'keymap
         (let ((map (make-sparse-keymap)))
           (define-key map [mode-line mouse-1] 'org-capture-refile)
           map))))

(defun eh-termux-create-mode-line ()
  (cond ((and (boundp 'org-capture-mode)
              org-capture-mode)
         (eh-termux-capture-mode-line))
        (t (eh-termux-default-mode-line))))

(defun eh-termux-enable ()
  (interactive)
  (setq-default truncate-lines t)
  (setq-default header-line-format nil)
  (setq-default mode-line-format
                '(:eval (eh-termux-create-mode-line)))
  (add-hook 'buffer-list-update-hook
            #'(lambda ()
                (setq header-line-format nil)
                (setq mode-line-format
                      '(:eval (eh-termux-create-mode-line)))))
  (add-hook 'org-capture-mode-hook
            #'(lambda ()
                (setq-local header-line-format nil))))

;; * Footer
(provide 'eh-termux)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-termux.el ends here
