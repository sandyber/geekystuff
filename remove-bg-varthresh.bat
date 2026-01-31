@echo off
setlocal enabledelayedexpansion

REM --- 1. SET DEFAULTS ---
set "INPUT_FILE=%~1"
set "BASE_NAME=%~n1"
set "WHITE_VAL=40"
set "RAW_SPEC="

if "%INPUT_FILE%"=="" (
    echo Usage: %~nx0 filename.pdf 1,2,3-10 threshold:60
    goto :eof
)

REM --- 2. THE ARGUMENT FILTER ---
shift
:arg_parse
if "%~1"=="" goto :args_done
set "TEMP_ARG=%~1"

REM Check for "threshold" (case insensitive)
echo !TEMP_ARG! | findstr /I "threshold" >nul
if !errorlevel! equ 0 (
    REM Split on colon or equals
    for /f "tokens=2 delims=:=" %%v in ("!TEMP_ARG!") do set "WHITE_VAL=%%v"
) else (
    REM Everything else is a page specification
    if "!RAW_SPEC!"=="" (set "RAW_SPEC=!TEMP_ARG!") else (set "RAW_SPEC=!RAW_SPEC! !TEMP_ARG!")
)
shift
goto :arg_parse

:args_done
REM Replace commas with spaces so the FOR loop can iterate correctly
set "CLEAN_SPEC=!RAW_SPEC:,= !"

echo [SETTINGS] Threshold: !WHITE_VAL!%%
echo [SETTINGS] Pages: !CLEAN_SPEC!

REM --- 3. CLEANUP ---
if not exist "final_pdf_pages" mkdir "final_pdf_pages"
if exist "final_pdf_pages\*.pdf" del /q "final_pdf_pages\*.pdf" 2>nul

REM --- 4. THE PARSER ENGINE ---
for %%P in (!CLEAN_SPEC!) do (
    set "ITEM=%%P"
    if not "!ITEM!"=="" (
        echo !ITEM! | findstr /C:"-" >nul
        if !errorlevel! equ 0 (
            for /f "tokens=1,2 delims=-" %%a in ("!ITEM!") do (
                for /L %%i in (%%a,1,%%b) do call :process_page %%i
            )
        ) else (
            call :process_page !ITEM!
        )
    )
)
goto :merge

:process_page
set "P_NUM=%1"
set "PADDED=000%P_NUM%"
set "PADDED=!PADDED:~-3!"
echo Processing Page !P_NUM! with !WHITE_VAL!%% Threshold...

gswin64c -sDEVICE=pnggray -r300 -dFirstPage=!P_NUM! -dLastPage=!P_NUM! -o "temp_page.png" -q -dBATCH -dNOPAUSE "%INPUT_FILE%"

if exist "temp_page.png" (
    magick "temp_page.png" -white-threshold !WHITE_VAL!%% -colorspace gray -type bilevel -compress fax "temp_cleaned.png"
    tesseract "temp_cleaned.png" "final_pdf_pages\page_!PADDED!" -l eng pdf
    del "temp_page.png" "temp_cleaned.png"
)
exit /b

:merge
echo.
echo Merging into %BASE_NAME%_cleaned.pdf...

REM 1. Create a simplified metadata file
(
echo [ /Title (%BASE_NAME%_Cleaned^)
echo   /Author (YSB-LLM^)
echo   /DOCINFO pdfmark
) > "docinfo.ps"

REM 2. Create the file list with QUOTES to prevent "undefined" errors
if exist "pages.txt" del "pages.txt"
for /f "tokens=*" %%f in ('dir /b /on "final_pdf_pages\page_*.pdf"') do (
    echo "final_pdf_pages/%%f" >> "pages.txt"
)

REM 3. Run Ghostscript using the @ syntax
if exist "pages.txt" (
    gswin64c -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dQUIET ^
    -sOutputFile="%BASE_NAME%_cleaned.pdf" ^
    @"pages.txt" "docinfo.ps"
    
    del "pages.txt" "docinfo.ps"
    echo Success! Created %BASE_NAME%_cleaned.pdf
) else (
    echo Error: No pages were found to merge.
)
pause