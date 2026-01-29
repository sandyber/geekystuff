@echo off
setlocal DisableDelayedExpansion
chcp 65001 > nul

if "%~1"=="" (
    echo [ERROR] No PDF file specified.
    pause
    exit /b 1
)

set "INPUT_FILE=%~1"
set "BASENAME=%~n1"
set "TARGET_DIR=%BASENAME%"
set "TEMP_LIST=parts_list.txt"
set "sequence=1"

REM Create the folder named after the PDF
if not exist "%TARGET_DIR%" mkdir "%TARGET_DIR%"

echo [STEP 1] Extracting data using parser.awk...
:: Run the merged parser
pdftk "%INPUT_FILE%" dump_data | awk -f parser.awk > "%TEMP_LIST%"

echo [STEP 2] Processing segments...
echo --------------------------------------------------

for /f "usebackq tokens=1-3 delims=;" %%a in ("%TEMP_LIST%") do (
    set "TITLE=%%a"
    set "START_PG=%%b"
    set "END_PG=%%c"

    call :ProcessSegment
)

if exist "%TEMP_LIST%" del "%TEMP_LIST%"
echo --------------------------------------------------
echo [DONE] Task accomplished.
echo Files saved in folder: "%TARGET_DIR%"
pause
goto :eof

:ProcessSegment
:: The title is already clean from the AWK script
set "OUT_NAME=%TARGET_DIR%\%sequence%-%TITLE%.pdf"

echo [WORKING] Section: "%TITLE%" (Pages %START_PG% to %END_PG%)

pdftk "%INPUT_FILE%" cat %START_PG%-%END_PG% output "%OUT_NAME%"

set /a sequence+=1
goto :eof