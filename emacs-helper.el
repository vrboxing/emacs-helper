;;; emacs-helper.el --- An emacs config collection for Non programmers

;; * Header
;; Copyright 2016 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; * Emacs-helper 使用说明                                          :README:doc:
;; ** 简介
;; Emacs-helper 是 [[https://github.com/tumashu][Tumashu]] 同学的个人配置，
;; 可以做为 Emacs 中文用户的一个参考。

;; ** 使用方法
;; 1. 下载: [[https://github.com/tumashu/emacs-helper/archive/master.zip][emacs-helper]]
;; 2. 解压缩到目录："d:/projects/emacs-packages/emacs-helper"
;; 3. 将下面的代码粘贴到 "~/.emacs"
;;    #+BEGIN_EXAMPLE
;;    (add-to-list 'load-path "d:/projects/emacs-packages/emacs-helper")
;;    (setq use-package-always-ensure t)
;;    (setq eh-enable-load-path-update nil)
;;    (setq eh-enable-full-install t)
;;    (require 'emacs-helper)
;;    #+END_EXAMPLE
;; 4. 重启 Emacs, 第一次加载 Emacs-helper 的时候，它依赖的包会自动安装。

;; * 代码                                                                 :code:
(load-library "eh-basic")
(load-library "eh-org")
(load-library "eh-bbdb3")
(load-library "eh-complete")
(load-library "eh-functions")
(load-library "eh-elfeed")
(load-library "eh-misc")

;; * Footer
(provide 'emacs-helper)

;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:

;;; emacs-helper.el ends here
