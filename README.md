- [Emacs-helper 使用说明](#org6a93d6b)
  - [简介](#orgf474132)
  - [使用方法（以 window 系统为例）](#org7cacfc4)


<a id="org6a93d6b"></a>

# Emacs-helper 使用说明


<a id="orgf474132"></a>

## 简介

Emacs-helper 是 [Tumashu](https://github.com/tumashu) 同学的个人配置， 可以做为 Emacs 中文用户的一个参考。


<a id="org7cacfc4"></a>

## 使用方法（以 window 系统为例）

1.  下载 [Emacs](https://ftp.gnu.org/gnu/emacs/windows/) .
2.  下载 [emacs-helper 压缩包](https://github.com/tumashu/emacs-helper/archive/master.zip) .
3.  将压缩包解压缩到任意一个目录，比如："d:/emacs-helper"
4.  用 emacs 打开 installer 文件： "d:/emacs-helper/eh-installer.el"
5.  运行下面的命令来安装 "d:/emacs-helper/elpa" 目录下所有的包， 并在 "~/.emacs" 文件中插入相应的配置片断。

        M-x eval-buffer
6.  重启 Emacs


Converted from emacs-helper.el by [el2org](https://github.com/tumashu/el2org) .