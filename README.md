# Raven
Raven is a Package Manager for Chez Scheme

***[Web Site](http://ravensc.com)***

***[Manual](https://guenchi.gitbooks.io/raven/content/)***

***[Packages List](http://ravensc.com/list)***

 package | description |  only for Chez | r6rs common | pure Scheme | C lib depenced
---------|-------------|----------------|-------------|-------------|----------------
***library***
darkart|foreign library call|X|||X
srfi | srfi || X | X |
sufage | srfi || X | X |
core | small procedures || X | X |
slib ||| X | X |
scheme-lib || X ||| X
***web***
igropyr | http server | X ||| X
ballista | web framwork | X || X | X 
catapult | web framwork | X || X | X 
libra | web framwork || X | X |
json | Json parser || X | X |
chez-json | Json parser || X | X |
anb-json | Json parser || X | X |
jwt | Json Web Token | X || X | X
liber | HTML parser || X | X |
***system***
socket || X ||| X
libc || X ||| X
***database***
mysql|| X ||| X 
***hash***
base64 | BASE64 || X | X |
csha256 | SHA256 hash | X |||X
***crypo***
chs256 | HMAC-SHA256 | X |||X
***compiler***
nanopass|compiler framwork||X|X|
lalr | LALR(1) parser || X | X |

`the Raven libraries generally work fine on the R6RS implementation, except for some built on the c library which dependent on Chez Scheme's FFI.` 


Running project: 

```
$ raven run example.sc
```

if you will run a project compiled:

```
$ export CHEZSCHEMELIBDIRS=.:lib:/usr/local/lib 
$ export CHEZSCHEMELIBEXTS=.chezscheme.sls::.chezscheme.so:.ss::.so:.sls::.so:.scm::.so:.sch::.so:.sc::.so
$ scheme exemple.so
```



The project Raven is supported by [theschemer.org](http://theschemer.org)

The principal developers are: [guenchi](https://github.com/guenchi), [chclock](https://github.com/chclock)

Nota Bene: We recommend that the libraries for Raven files use .sc for distinguishing the libraries of r5rs / r7rs small.




