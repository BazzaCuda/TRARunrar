# TRAR
 
This complete re-envisioning, re-architecting and re-writing of Phillipe Wechsler's 2008/2009 Delphi component now supports both the 32-bit and 64-bit versions of unrar.dll from rarlab.com

listArchive, testArchive and extractArchive tested and working against the latest versions of unrar.dll (v7.11.1.1525 - 2025/03/19), including multi-volume RAR archives.

Full PDF documentation to follow.

TRAR as a Delphi IDE component has now been added. 
TRAR works just as well as standalone, included (i.e. "uses") units in a Delphi project. 
TRAR as a Delphi IDE component has been included in keeping with Phillipe's original.

A basic test project is included which will be enhanced based on feedback.

**Updates, comments and suggestions always welcome on the [Issues](https://github.com/BazzaCuda/TRARunrar/issues) page.**

My Extractor application (https://github.com/BazzaCuda/Extractor) contains a wrapper for 7z.dll. TRAR will be incorporated into that application at some point (primarily to support multi-volume RAR archives) and will therefore serve as an additional test project for TRAR.

Of course, big thanks are due to Phillipe for his original 32-bit component, some 16 years ago!

N.B. The code expects you to rename unrar.dll to either unrar32.dll or unrar64.dll as appropriate; both are included for download in this project's releases.
