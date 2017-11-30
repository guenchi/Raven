# Raven
Raven is a Package Manager of Scheme
ravensc.com

Raven由theschemer.org社区开发和维护

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

3.b 命令 raven install LIBNAME 

从 ravensc.com/库名／版本号 下载库到项目下lib文件夹，并将其写入package.json文件。

3.c 命令 raven uninstall LIBNAME 

从lib文件夹删除库，并将其从package.json文件删除。

4.d 选项 -dev

安装和卸载时选项，区别在于写入json时加入“devDependencies”

package.json样例
{
  "name": "",
  "version": "1.0.0",
  "description": "",
  "author": "",
  "private": true,
  "scripts": {
    "dev": "",
    "build": "",
    "g": "",
    "start": "",
    "precommit": "",
    "lint": ""
  },
  "dependencies": {
    "LIBNAME": "^0.15.3",
  },
  "devDependencies": {
    "LIBNAME": "^7.1.1",
  }
}
