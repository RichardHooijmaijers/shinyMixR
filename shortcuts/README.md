# Readme

This folder includes bash/batch scripts to start the shinyMixR application from a file explorer. There are versions for mac (start_shinyMixR_Mac.command), windows (start_shinyMixR_Win.bat) and linux (start_shinyMixR_Lin). You can place the files in a root folder were multiple shinyMixR projects are present. By double clicking the files, the shinyMixR application will start and you can navigate to the applicable project.

There are a couple of things to take into account when working with these files:

- On windows the shortcut will look for an R version in "Program Files" and "Program Files (x86)"
- It is assumed that R is added to the PATH environment variable. In case this is not the case (e.g. not by default in windows), it should be added to the path manually or the line that starts with 'Rscript' should be changed to include the full path (e.g. "C:\\Program files\\R\\R3.4.4\\bin\\Rscript.exe")
- The files will start the application from the command line, this means a terminal is opened during usage and should be closed manually when the application is closed
- For linux it might be necessary to edit the file preferences to run executable text files when they are opened
