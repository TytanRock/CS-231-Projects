@echo off


if NOT EXIST %1\build\ (
    mkdir %1\build\
    echo Making Build Directory
)
if NOT EXIST %1\bin\ (
    mkdir %1\bin\
    echo Making Bin Directory
)

cd %1

ghc --make %2 -outputdir bin\ -o build\main.exe

if %ERRORLEVEL% NEQ 0 (
    echo Build Errors detected 
) else (
    echo Build Succeeded 
)