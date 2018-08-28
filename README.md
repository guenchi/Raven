# Raven
Raven is a Package Manager for Chez Scheme

***Web Site:*** http://ravensc.com

***Packages List***  http://ravensc.com/list

 package | description |  only for Chez | r6rs common | pure Scheme | C lib depenced
---------|-------------|----------------|-------------|-------------|----------------
***collection*** 
srfi | srfi || X | X |
sufage | srfi || X | X |
slib ||| X || X
scheme-lib ||| X || X 
***web***
igropyr | http server | X ||| X
ballista | web framwork | X || X |
catapult | web framwork | X || X |
json | Json parser || X | X |
chez-json | Json parser || X | X |
anb-json | Json parser || X | X |
jwt | Json Web Token | X ||| X
liber | HTML parser || X | X |
***database***
mysql|| X ||| X 
***hash***
base64 | BASE64 || X | X |
csha256 | SHA256 hash | X |||X
***crypo***
chs256 | HMAC-SHA256 | X |||X
***generator***
lalr | LALR(0) || X | X |

the Raven libraries generally work fine on the R6RS implatetion, except for some built on the c library which dependent on Chez Scheme's FFI interface. 


The project Raven is surport by http://theschemer.org

The principal developers are: guenchi, chclock

Nota Bene: We recommand the libraries of Raven files use .sc for disdiguesh the libraries of r5rs / r7rs small.

Manual: https://guenchi.gitbooks.io/raven/content/



