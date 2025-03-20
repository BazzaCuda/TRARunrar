# TRAR
 
This complete re-envisioning, re-architecting and re-writing of Phillipe Wechsler's 2008/2009 Delphi component now supports both the 32-bit and 64-bit versions of unrar.dll from rarlab.com

listArchive, testArchive and extractArchive tested and working against the latest versions of unrar.dll (v7.11.1.1525 - 2025/03/19), including multi-volume RAR archives.

Full documentation to follow: see below for usage examples.

TRAR as a Delphi IDE component has now been added. 
TRAR works just as well when used as standalone, included (i.e. "uses") units in a Delphi project. 
TRAR as a Delphi IDE component has been included in keeping with Phillipe's original.

A basic test project is included which will be enhanced based on feedback. TRAR has been, and continues to be, extensively used and tested in my own projects, which are not currently open source. I hope to rectify that in due course.

**Updates, comments and suggestions always welcome on the [Issues](https://github.com/BazzaCuda/TRARunrar/issues) page.**

My Extractor application (https://github.com/BazzaCuda/Extractor) contains a wrapper for 7z.dll. TRAR will be incorporated into that application at some point (primarily to support multi-volume RAR archives) and will therefore serve as an additional test project for TRAR.

Of course, big thanks are due to Phillipe for his original 32-bit component, some 16 years ago!

N.B. The code expects you to rename unrar.dll to either unrar32.dll or unrar64.dll, as appropriate, so you can easily switch between compiling and running your application for either architecture; both are included for download in this project's releases.

-----------

Example usage:

```Delphi
uses
  RAR, RAR_DLL;
```

Testing a RAR archive: 
```Delphi
case RAR.testArchive(archivePath) of FALSE: showMessage('Test Failed!'); end;
```

Getting information about each file in a RAR archive:
```Delphi

  TRARFileItem = record // defined in TRAR.pas
    fileName:             AnsiString;
    fileNameW:            WideString;
    splitFile:            boolean;
    compressedSize:       cardinal;
    unCompressedSize:     cardinal;
    hostOS:               string;
    CRC32:                string;
    attributes:           cardinal;
    comment:              AnsiString;
    time:                 TDateTime;
    compressionStrength:  cardinal;
    archiverVersion:      cardinal;
    encrypted:            boolean;
    hash:                 string;
  end;

  ...

  RAR.onListFile := RARListFile; // must be procedure of object

  ...

  procedure TIndexer.RARListFile(const fileItem: TRARFileItem);
  begin
    memo1.lines.add(fileItem.fileName);
  end;

  ...

  RAR.listArchive(archivePath); // calls onListFile for each file in the RAR archive
 
```

Extracting the entire RAR archive to a folder:
```Delphi
  RAR.extractArchive(archiveFile, extractPath); // TRAR will "forceDirectories()" the extraction path as necessary
```

Extracting an individual file from a RAR archive:
```Delphi
  RAR.extractArchive(archiveFile, extractPath, filePath); // filePath is the entire path to the file _inside_ the RAR archive, including any subfolders
```

Extracting a list of files from a RAR archive:
```Delphi
  RAR.clearFiles;
  RAR.prepareArchive(archiveFile); // reduces the internal overheads involved in repeated extractions
  
  for i := 0 to memo1.lines.count - 1 do
    RAR.addFile(memo1.lines[i]);

  showMessage(format('Extracting %d files', [RAR.fileCount]));
    
  RAR.extractPreparedArchive(archiveFile, extractPath);
  RAR.clearFiles; // good practice as this list takes precedence over specifyig an individual file with RAR.extractArchive(archiveFile, extractPath, filePath);
```
