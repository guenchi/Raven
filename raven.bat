@echo off
@set CHEZSCHEMELIBDIRS=.;./lib;%UserProfile%\raven
@set "EXE=%UserProfile%\raven\raven\raven.sc"
scheme --script %EXE% %*