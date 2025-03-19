# TRAR
 
This complete rewrite of Phillipe Wechsler's 2008/2009 Delphi component is still work-in-progress, but TRAR now supports both the 32-bit and 64-bit versions of unrar.dll from rarlab.com

listFiles, testArchive and extractArchive tested and working against the latest versions of unrar.dll (v7.11.1.1525 - 2025/03/19).

TRAR as a Delphi IDE component has now been added.

A basic test project is included which will be enhanced based on feedback.

**Updates, comments and suggestions always welcome on the [Issues](https://github.com/BazzaCuda/TRARunrar/issues) page.**

My Extractor application (https://github.com/BazzaCuda/Extractor) contains a wrapper for 7z.dll. TRAR will be incorporated into that application at some point (primarily to support multi-volume RAR archives) and will therefore serve as another test project for TRAR.

Of course, big thanks are due to Phillipe for his original 32-bit component, some 16 years ago!

N.B. The code expects you to rename unrar.dll to either unrar32.dll or unrar64.dll as appropriate; both are included for download in this project's assets.
