@echo off

if NOT EXIST %1\build\ (
    mkdir %1\build\
    echo Making Directory
)
gcc %1\main.c -o %1\build\main.exe