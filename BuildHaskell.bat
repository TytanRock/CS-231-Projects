@echo off


if NOT EXIST %1\build\ (
    mkdir %1\build\
    echo Making Build Directory
)
if NOT EXIST %1\bin\ (
    mkdir %1\bin\
    echo Making Bin Directory
)
ghc %1\%2 -outputdir %1\build\ -o %1\bin\main.exe

if %ERRORLEVEL% NEQ 0 (
    echo Build Errors detected 
) else (
    echo Build Succeeded 
)