@echo off
set COQTOP=C:\progra~2\Coq
set COQLIB=%COQTOP%\lib
set COQBIN=%COQTOP%\bin
set PATH=%PATH%;%COQBIN%
set HOME=%HOMEPATH%
%COQBIN%\coqtop.exe -top "%COQTOP%" -emacs-U
