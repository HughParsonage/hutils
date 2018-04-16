$filepath = "."
$filetype = "*.*"
$file_count = [System.IO.Directory]::GetFiles("$filepath", "$filetype").Count
