#!/bin/awk -f
BEGIN {
    FS=": "
    OFS=";"
}
/^BookmarkBegin/ {
  if (this_level == 1) { # Only handle high-level or low-level sections
    next
  }
  if (this_page == "" || this_title == ""){
    next
  }
  if (last_page != "" && last_title != "") {
    print last_title, last_page, this_page-1
  }
  last_title=this_title
  last_page=this_page
  this_title=this_page=this_level=""
}
/^BookmarkTitle:/ {
    this_title=$2
}
/^BookmarkPageNumber:/ {
    this_page=$2
}
/^BookmarkLevel:/ {
    this_level=$2
}
END {
  print last_title, last_page, this_page-1
  print this_title, this_page, "end"
}

{
    # This is the merged cleaning logic:
    # It removes stars (*), daggers (†), colons (:), quotes ("), etc.
    gsub(/[*†:?"<>|]/, "", title); 
    
    # Optional: Clean up leading/trailing spaces
    gsub(/^[ \t]+|[ \t]+$/, "", title);

    # Print the clean output for the Batch file to read
    print title ";" start_page ";" end_page;
}