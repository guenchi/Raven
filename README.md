# Raven
Raven is a Package Manager for Chez Scheme

Officiel Web Site: http://ravensc.com

Packages List: http://ravensc.com/list

The project Raven is surport by http://theschemer.org


the Raven libraries generally work fine on the R6RS implatetion, except for some built on the c library which dependent on Chez Scheme's FFI interface. 


***Packages List***

Package | description |  only for Chez | r6rs common | pure Scheme | with C lib
--------|-------------|----------------|-------------|-------------|------------
COLLECTION 
scheme-lib ||| X || X         
slib ||| X || X
SERVER
igropyr | http server | X ||| X
HASH
base64 | BASE64 || X | X ||
csha256 | SHA256 hash | X |||X
chs256 | HMAC-SHA256 | X |||X



The principal developers are: guenchi, chclock

Nota Bene: We recommand the libraries of Raven files use .sc for disdiguesh the libraries of r5rs / r7rs small.

Manual: https://guenchi.gitbooks.io/raven/content/



