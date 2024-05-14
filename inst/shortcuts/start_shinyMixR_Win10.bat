echo off()
FOR /F "delims=" %i IN ('dir "C:\Program Files\R" /b /ad-h /t:c /od') DO SET a=%i
IF "%a%"=="" (FOR /F "delims=" %i IN ('dir "C:\Program Files (x86)\R" /b /ad-h /t:c /od') DO SET a=%i)
pause
IF EXIST "C:\Program Files\R\%a%\bin\x64\" (
    "C:\Program Files\R\%a%\bin\x64\Rscript.exe" -e "library(shinyMixR);run_shinymixr(launch.browser=TRUE)"
) ELSE (
    "C:\Program Files\R\%a%\bin\i386\Rscript.exe" -e "library(shinyMixR);run_shinymixr(launch.browser=TRUE)"
)
