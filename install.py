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
target_dir = "/usr/local/lib/raven/raven"


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
    os.remove("install.py")
    print("raven install success")

def win_proc():
    "windows file procedure"
    target_dir = os.path.join(os.environ.get("UserProfile"), ".raven\\raven")
    if not os.path.exists(target_dir):
        os.makedirs(target_dir)
    shutil.copy(tmp, os.path.join(target_dir, "raven.sc"))
    with open(os.path.join(target_dir, "raven.bat"), 'w') as outfile:
        outfile.write("@echo off\n@set CHEZSCHEMELIBDIRS=.;./lib;%UserProfile%/.raven\nscheme --script raven.sc %*")
    print("The script has been downloaded in %s.\nYou should add this path to the system variables %%PATH%% before you enjoy the raven."%(target_dir))

def linux_proc():
    "linux file procedure"
    if not os.path.exists(target_dir):
        os.makedirs(target_dir)
    target_file = os.path.join(target_dir, "raven.sc")
    shutil.copy(tmp, target_file)
    os.remove("/usr/local/bin/raven")
    os.system('ln -s /usr/local/lib/raven/raven/raven.sc /usr/local/bin/raven')
    os.system('chmod +x /usr/local/bin/raven')
    

if __name__ == "__main__":
    main()
