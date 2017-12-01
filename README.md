# Raven
Raven is a Package Manager of Scheme

官方网站 ravensc.com

#注意：Raven项目建议使用sc作为后缀，表明不加修改即可运行于支持 r6rs 版本实现（未来的r7rs large）以区别于其他后缀的 r5rs ／ r7rs small实现。

Raven由theschemer.org社区开发和维护

工作原理 仿造node的npm

todo:

1. Raven自身构造

Raven自身为一段通过Chez Scheme解释器运行的脚本文件。

2. 安装Raven的脚本 （功能需求）

此脚本文件可以用python/ruby 或bash编写, 
将Raven主体文件拷贝入 /usr/local/bin, 写入执行环境变量，给予执行权限。

3. 工作原理

3.a 命令 raven install

读入命令当前文件夹下的`package.json`文件，并取得其中 `dependencies`键包含依赖散列表。  
除去重复依赖项，从 ravensc.com/库名/版本号 依次下载依赖库到项目下lib文件夹。

3.b 命令 raven install LIBNAME 

从 ravensc.com/库名/版本号 下载库到项目下`lib`文件夹，并将其写入`package.json`文件。

3.c 命令 raven uninstall LIBNAME 

从`lib`文件夹删除库，并将其从`package.json`文件删除。

4.d 选项 -dev

安装和卸载时选项，区别在于写入json时加入`devDependencies`

`package.json`样例

```json
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
```
