#!/usr/bin/python3
from __future__ import print_function, unicode_literals
import platform, os, shutil, sys
if sys.version > '3':
    from urllib import request
else:
    import urllib


url = "http://ravensc.com/raven.sc"
tmp = "raven.tmp"
sysstr = platform.system()
target_dir = "/usr/local/bin"


def main():
    "download raven file"
    if sys.version > '3':
        request.urlretrieve(url, tmp)
    else:
        urllib.urlretrieve(url, tmp)
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
            outfile.write("@echo off\n@set CHEZSCHEMELIBDIRS=.;./lib;%path%\nscheme --script raven %*")
    print("The script has been downloaded to Raven folder, you can move it to anywhere.\n\
At last, you should add the Raven path into the environment variable PATH!")

def linux_proc():
    "linux file procedure"
    target_file = os.path.join(target_dir, "raven")
    shutil.copy(tmp, target_file)
    os.system('chmod a+x /usr/local/bin/raven')


if __name__ == "__main__":
    main()
