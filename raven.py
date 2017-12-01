#!/usr/bin/python3
import platform, os, stat, shutil
from urllib import request

url = "http://ravensc.com/raven.sc"
tmp = "raven.tmp"
sysstr = platform.system()
target_dir = "/usr/local/bin"

def main():
    "download raven file"
    with request.urlopen(url) as web:
        with open(tmp, 'wb') as outfile:
            outfile.write(web.read())
    if sysstr == "Windows":
        win_proc()
    else:
        linux_proc()
    os.remove(tmp)
    os.remove("raven.py")
    print("raven install success")

def win_proc():
    "windows file procedure"
    target_dir = "./Raven"
    if not os.path.exists(target_dir):
        os.makedirs(target_dir)
    shutil.copy(tmp, os.path.join(target_dir, "raven"))
    with open(os.path.join(target_dir, "raven.bat"), 'w') as outfile:
            outfile.write("scheme --script raven %1 %2 %3 %4 %5 %6 %7 %8 %9")
    print("脚本已下载至Raven文件夹, 你可以将其移动至任意位置, 最后将Raven路径添加至环境变量PATH")

def linux_proc():
    "linux file procedure"
    target_file = os.path.join(target_dir, "raven")
    shutil.copy(tmp, target_file)
    os.system('chmod a+x /usr/local/bin/raven')


if __name__ == "__main__":
    main()
