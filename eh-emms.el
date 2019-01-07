;;; eh-emms.el --- Tumashu's emacs configuation

;; * Header
;; Copyright (c) 2011-2019, Feng Shu

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
(use-package emms
  :config
  (use-package emms-setup :ensure nil)
  (use-package emms-info-libtag :ensure nil)
  (use-package dired :ensure nil)

  (emms-all)
  (emms-default-players)
  (when (fboundp 'emms-cache)
    (emms-cache 1))

  ;; EMMS 目录
  (setq emms-source-file-default-directory "~/Music")
  (setq emms-directory "~/emacs.d/emms/")

  (unless (file-directory-p emms-source-file-default-directory)
    (make-directory
     (file-name-as-directory
      emms-source-file-default-directory) t))

  (unless (file-directory-p emms-directory)
    (make-directory
     (file-name-as-directory emms-directory) t))

  ;; 设定EMMS主模式为 Playlist 模式
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)

  ;; 修复播放完后的 BUG
  (setq emms-player-next-function 'emms-next)

  ;; 设定音轨初始化信息
  (add-to-list 'emms-track-initialize-functions
               'emms-info-initialize-track)

  ;; 关闭 EMMS 信息异步模式
  (setq emms-info-asynchronously nil)

  ;; 设定 EMMS 启动列表循环播放
  (setq emms-repeat-playlist t)

  ;; 排序方法: 艺术家 -> 专辑 -> 序号
  (setq emms-playlist-sort-function
        'emms-playlist-sort-by-natural-order)

  ;; 在 minibuffer 中显示播放信息 (emms-show)
  (setq emms-show-format "正在播放: %s")
  (add-hook 'emms-player-started-hook #'emms-show)

  ;; 不在 mode-line 中显示播放信息
  (emms-mode-line -1)

  ;; 如何显示 track
  (setq emms-track-description-function
        #'eh-emms-track-simple-description)

  (defun eh-emms-track-simple-description (track)
    (let ((type (emms-track-type track)))
      (concat "♪ "
              (cond ((eq 'file type)
                     (file-relative-name
                      (emms-track-name track)
                      emms-source-file-default-directory))
                    ((eq 'url type)
                     (emms-format-url-track-name (emms-track-name track)))
                    (t (concat (symbol-name type)
                               ": " (emms-track-name track)))))))

  ;; 显示歌词
  (emms-lyrics 1)

  ;; Track information, function `emms-info-libtag' need emms-print-metadata
  ;; command in emms, which build with: make emms-print-metadata
  (setq emms-info-functions '(emms-info-libtag))

  ;; 设置 Playlist 的显示方式
  (setq emms-last-played-format-alist
        '(((emms-last-played-seconds-today) . "%H:%M")
          (604800                           . "%H:%M")
          ((emms-last-played-seconds-month) . "%d")
          ((emms-last-played-seconds-year)  . "%m-%d")
          (t                                . "%Y")))

  ;; 设置 EMMS 浏览器, 默认显示方式为: 显示所有
  (emms-browser-set-filter (assoc "EVERYTHING" emms-browser-filters))

  ;; filter: 显示所有
  (emms-browser-make-filter "EVERYTHING" 'ignore)

  ;; filter: 只显示文件
  (emms-browser-make-filter
   "ALL-FILES"
   (emms-browser-filter-only-type 'file))

  ;; filter: 最近一个星期播放的
  (emms-browser-make-filter
   "LAST-WEEK"
   (emms-browser-filter-only-recent 7))

  ;; filter: 最近一个月都没有播放的
  (emms-browser-make-filter
   "LAST-MONTH-NOT-PLAYED"
   (lambda (track)
     (not (funcall (emms-browser-filter-only-recent 30) track))))

  ;; 设置 emms browser 和 browser-list 中音乐的显示格式
  (setq emms-browser-info-year-format      "%i+ %n")
  (setq emms-browser-info-genre-format     "%i+ %n")
  (setq emms-browser-info-performer-format "%i+ %n")
  (setq emms-browser-info-composer-format  "%i+ %n")
  (setq emms-browser-info-artist-format    "%i* %n")
  (setq emms-browser-info-album-format     "%i- %n")
  (setq emms-browser-info-title-format     "%i♪ %n")
  (setq emms-browser-playlist-info-year-format      "%i+ %n")
  (setq emms-browser-playlist-info-genre-format     "%i+ %n")
  (setq emms-browser-playlist-info-performer-format "%i+ %n")
  (setq emms-browser-playlist-info-composer-format  "%i+ %n")
  (setq emms-browser-playlist-info-artist-format    "%i* %n")
  (setq emms-browser-playlist-info-album-format     "%i- %n")
  (setq emms-browser-playlist-info-title-format     "%i♪ %n"))

;; * Footer
(provide 'eh-emms)

;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:

;;; eh-emms.el ends here
