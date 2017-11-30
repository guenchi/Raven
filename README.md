# Raven
Raven is a Package Manager of Scheme
ravensc.com

工作原理 仿造node的npm

todo：

1. Raven自身构造

Raven自身为一段通过Chez Scheme解释器运行的脚本文件。

2. 安装Raven的脚本 （功能需求）

此脚本文件可以用python / ruby 或bash编写
将Raven主体文件拷贝入 /usr/local/bin, 写入执行环境变量，给予执行权限。

3. 工作原理

3.a 命令 raven install

读入命令当前文件夹下的package.json文件，并取得其中 “dependencies” 键 包含依赖散列表。
除去重复依赖项，从 ravensc.com/库名／版本号 依次下载依赖库到项目下lib文件夹。
