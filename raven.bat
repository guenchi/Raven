@echo off
@set CHEZSCHEMELIBDIRS=.;lib;%UserProfile%\raven
@set CHEZSCHEMELIBEXTS=.chezscheme.sls;;.chezscheme.so;.ss;;.so;.sls;;.so;.scm;;.so;.sch;;.so;.sc;;.so
@set "EXE=%UserProfile%\raven\raven\raven.sc"
scheme --script %EXE% %*