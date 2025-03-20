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

_N.B. If you download the DLL from rarlab.com, the code expects you to rename unrar.dll to either unrar32.dll or unrar64.dll, as appropriate, so that you can easily switch between compiling and running your application for either architecture; both DLLs are included for download in this project's releases._

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

  TRARFileItem = record // defined in RAR.pas
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

  procedure TIndexer.RARListFile(const aFileItem: TRARFileItem);
  begin
    memo1.lines.add(aFileItem.fileName);
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

Providing a password:

You can either provide a password up-front before calling the RAR operation, or you can provide the password when TRAR requests it.

Up-Front:
```Delphi
  RAR.password := 'this is the pw';
  RAR.testArchive(archivePath);
```

On Request:
```Delphi
  RAR.onPasswordRequired := RARPasswordRequired; // must be procedure of object

  ...

  procedure TIndexer.RARPasswordRequired(const aFileName: AnsiString; out oNewPassword: AnsiString; out oCancel: boolean);
  begin
    case aFileName = 'I_know_this_pw.txt' of  TRUE: oNewPassword := 'this is the password';
                                             FALSE: oCancel      := TRUE;
    end;
  end;
```

Receiving error codes:
```Delphi
  RAR.onError := RARError; // must be procedure of object
  ...
  // the error codes and operations are defined in RAR_DLL.pas
  procedure TForm1.RAR1Error(Sender: TObject; const aErrorCode: Integer; const aOperation: TRAROperation);
  begin
    lblError.caption := format('code: %d, operation: %d', [aErrorCode, aOperation]);
  end;
```

Receiving feedback during RAR operations:
```Delphi
  TRARProgressInfo = record // defined in RAR.pas
    fileName:           WideString; // the full path to the file within the RAR archive
    archiveBytesTotal:  LongInt;
    archiveBytesDone:   LongInt;
    fileBytesTotal:     LongInt;
    fileBytesDone:      LongInt;
  end;

  RAR.onProgress := RARProgress; // must be procedure of object
  ...
  // this gets called periodically during UnRAR.dll's processing, typically after each 4MB chunk of data
  procedure TIndexer.RARProgress(Sender: TObject; const aProgressInfo: TRARProgressInfo);
  begin
    // Sender is the TRAR object instance
    // N.B. excessive updating of labels and progressBars can have a detrimental effect on operational speed
    //      especially when extracting a long list of files (see addFile() above) 
  end;
```

Multi-Volume RAR archives:

These days, all the files of a multi-volume RAR archive (archive.part1.rar, archive.part2.rar, etc) will typically be in the same folder together and named correctly.

If this is the case, TRAR and unrar.dll will handle multi-volume RAR archives automatically. This applies to RAR archives that are numbered starting with ...part1.rar, or ...part01.rar, or ...part001.rar, etc.

On the rare occasion that they're in different locations, or the secondary parts haven't all been named uniformally, you can get TRAR to ask you for the full path to the next volume:
```Delphi
  RAR.onNextVolRequired := RARNextVolRequired; // must be procedure of object;
  ...
  procedure TIndexer.RARNextVolRequired(Sender: TObject; const aRequiredFileName: Ansistring; out oNewFileName: Ansistring; out oCancel: boolean);
  begin
    // Sender is the TRAR object instance
    case aRequiredFileName = 'myarchive.part6.rar' of  TRUE: oNewFileName := 'C:\Windows\System32\Whats_It_Doing_Here.part006.rar';
                                                      FALSE: oCancel      := TRUE; // no idea where it is
    end;
  end;
```
  




  

