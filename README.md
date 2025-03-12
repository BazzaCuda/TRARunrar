# TRAR
 
This complete rewrite of Phillipe Wechsler's 2008/2009 Delphi component is still work-in-progress, but TRAR now supports both the 32-bit and 64-bit versions of unrar.dll from rarlab.com

listFiles and testArchive tested and working against the latest versions of unrar.dll (v7.11.1.1525 - 2025/03/03), but with the caveat that there is still some work to do when providing passwords to unrar; however, onPasswordRequired works.

Currently, all testing of rar.pas and rar_dll.pas is with them being included units in a project. Testing TRAR as an IDE component will be carried out when the rewrite is complete.

updates, comments and suggestions welcome.

My Extractor application (https://github.com/BazzaCuda/Extractor) contains a wrapper for 7z.dll. TRAR will be incorporated into that application at some point (primarily to support multi-volume RAR archives) and will therefore serve as the test project for TRAR.

Of course, big thanks are due to Phillipe for his original 32-bit component, some 16 years ago!

N.B. The code expects you to rename unrar.dll to either unrar32.dll or unrar64.dll as appropriate.
