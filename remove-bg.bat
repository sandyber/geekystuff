REM Removing gray bg in pdf files. Useful for archive.org files.
@echo off
setlocal enabledelayedexpansion

REM --- Argument Check ---
set "INPUT_FILE=%~1"
REM Get the name without extension for naming and metadata
set "BASE_NAME=%~n1"

if "%INPUT_FILE%"=="" (
    echo Usage: convert.bat filename.pdf 1,2,3-10
    goto :eof
)

REM --- Combine arguments after filename ---
shift
set "RAW_SPEC="
:argloop
if "%~1" neq "" (
    set "RAW_SPEC=!RAW_SPEC! %~1"
    shift
    goto :argloop
)
if "!RAW_SPEC!"=="" (
    echo Error: No page range specified.
    goto :eof
)

REM --- Cleanup & Setup ---
if exist "final_pdf_pages" (
    echo Cleaning up old files...
    del /q "final_pdf_pages\*.pdf"
) else (
    mkdir "final_pdf_pages"
)
if exist "file_list.txt" del "file_list.txt"
set "CLEAN_SPEC=!RAW_SPEC:,= !"

REM --- The Parser Engine ---
for %%P in (%CLEAN_SPEC%) do (
    set "ITEM=%%P"
    echo !ITEM! | findstr /C:"-" >nul
    if !errorlevel! equ 0 (
        for /f "tokens=1,2 delims=-" %%a in ("!ITEM!") do (
            set "START=%%a"
            set "END=%%b"
        )
        for /L %%i in (!START!,1,!END!) do (call :process_page %%i)
    ) else (
        call :process_page !ITEM!
    )
)
goto :merge

:process_page
set "P_NUM=%1"
set "TMP_NUM=00%P_NUM%"
set "PADDED=!TMP_NUM:~-3!"
echo Processing Page !P_NUM!...
gswin64c -sDEVICE=pnggray -r300 -dFirstPage=!P_NUM! -dLastPage=!P_NUM! -o "temp_page.png" -q -dBATCH -dNOPAUSE "%INPUT_FILE%"
REM timeout /t 1 /nobreak >nul
if exist "temp_page.png" (
    magick "temp_page.png" -white-threshold 40%% -colorspace gray -type bilevel -compress fax "temp_cleaned.png"
    REM OCR step
    tesseract "temp_cleaned.png" "final_pdf_pages\page_!PADDED!" -l eng pdf
    del "temp_page.png"
    del "temp_cleaned.png"
)
exit /b

:merge
echo.
echo Merging into %BASE_NAME%_cleaned.pdf with Metadata...

REM 1. Create a clean metadata file (using .ps extension to be safe)
echo [ /Title (%BASE_NAME%Cleaned) /Author (YSB-LLM) /DOCINFO pdfmark > "docinfo.ps"

REM 2. Create the standard file list for the pages ONLY
if exist "pages.txt" del "pages.txt"
for /f "tokens=*" %%f in ('dir /b /on "final_pdf_pages\page_*.pdf"') do (
    echo "final_pdf_pages\%%f" >> "pages.txt"
)

REM 3. Run Ghostscript: 
REM We pass the list of pages FIRST, then the metadata file LAST.
REM Ghostscript applies the metadata to the current output stream.
if exist "pages.txt" (
    gswin64c -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dQUIET ^
    -sOutputFile="%BASE_NAME%_cleaned.pdf" ^
    @pages.txt "docinfo.ps"
    
    del "docinfo.ps"
    del "pages.txt"
    echo Success! Metadata applied.
) else (
    echo Error: No pages were found to merge.
)
pause