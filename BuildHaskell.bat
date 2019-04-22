@echo off


if NOT EXIST %1\build\ (
    mkdir %1\build\
    echo Making Build Directory
)
if NOT EXIST %1\bin\ (
    mkdir %1\bin\
    echo Making Bin Directory
)
ghc --make %1\%2 -outputdir %1\bin\ -o %1\build\main.exe

if %ERRORLEVEL% NEQ 0 (
    echo Build Errors detected 
) else (
    echo Build Succeeded 
)