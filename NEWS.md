# shinyMixR 0.2

Within this version some major updates have been implemented, see below the most important changes:

- Complete new structure of the app using a template system for ui/server and the widgets. This was implemented to make it easier to maintain and extend the package
- Omit filebrowser functionality, this lead to confusion and works slowly. Also could not properly browse from root. This is now handled by providing location in run_shinymixr
- Switched from shinyBS to shiny for modals (latest shiny version does not play well with shinyBS!)
- Improved way of adapting meta data in overview widget
- Improved way of getting meta data from models (making startup of app appr. 10 times faster)
- Automatic output redirecting to app for run model and scripts
- Omitted duplicate model functionality as this practically the same as the save as functionality

# shinyMixR 0.1.4

Various bug fixes and new features are included. Most important adaptations are listed below:

- Various adaptations to work with the 1.0 version of nlmixr
- Inclusion of file browser, it is now possible to switch between projects using the 'Project selection' button. This button will open up a file browser to navigate to different projects. This is new functionality that does not break existing projects, so starting the app from the current location will show the project in that location
- Possibility to start the application using batch/bash scripts (available in the shortcut directory in github repo)
- Within the edit widget it is possible to use 'save as'
- Within the run model widget is is possible to in/exclude CWRES and NPDE in the output

# shinyMixR 0.1.3

First github release


# shinyMixR 0.1.1 - 0.1.2

Development versions of the package (not released on github)
