@echo off

if EXIST %1\build\main.exe (
    .\%1\build\main.exe
) else (
    echo "Executable hasn't been built"
)