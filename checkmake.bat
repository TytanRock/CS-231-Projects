echo off

if EXIST %1 (
    echo Directory Exists
) ELSE (
    mkdir %1
    echo Making Directory
)