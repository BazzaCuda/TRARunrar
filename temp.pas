unit temp;

interface

implementation



function TRAR.extractFiles(const aFilePath: AnsiString; bRestoreFolder: boolean; aFiles:TStrings): boolean;
var
  vReadFileHeaderResult:  integer;
  vExistentFile:          TRARReplaceData;
  vArchiveFile:           TRARReplaceData;
  ft:                     _FILETIME;
  st:                     TSystemTime;
  vReplaceResult:         TRARReplace;
begin
  FAbort := FALSE;
  result := openArchive(aFilePath, TRUE) <> RAR_INVALID_HANDLE;
  if FAbort or NOT (result) then EXIT;

//  if aFilePath[Length(aFilePath)] <> '\' then aFilePath := aFilePath+'\';
  FProgressInfo := default(TRARProgressInfo); // reset all counts to zero

  try
    RARSetCallback(FArchivehandle, UnRarCallBack, LPARAM(pointer(SELF)));

    if FPassword <> '' then RARSetPassword(FArchiveHandle, PAnsiChar(FPassword));

    vReadFileHeaderResult := RAR_SUCCESS;

    while (vReadFileHeaderResult = RAR_SUCCESS) and result do begin

      vReadFileHeaderResult := RARReadHeaderEx(FArchiveHandle, @FFileHeaderDataEx);
      if vReadFileHeaderResult = ERAR_END_ARCHIVE then BREAK;

      if vReadFileHeaderResult <> RAR_SUCCESS then  begin
                                                      result := FALSE;
                                                      checkRARResult(vReadFileHeaderResult, roListFiles);
                                                    end;

      FProgressInfo                     := default(TRARProgressInfo);
      FProgressInfo.FileName            := FFileHeaderDataEx.FileNameW;
      FProgressInfo.ArchiveBytesTotal   := FFileHeaderDataEx.UnpSize;
      vReplaceResult                    := rrOverWrite;

      if extractFile(strPas(FFileHeaderDataEx.FileName), aFiles) then begin    //todo: UniCode FileName

        if bRestoreFolder then
          vExistentFile.FileName  := aFilePath + strPas(FFileHeaderDataEx.FileName)
        else
          vExistentFile.FileName  := aFilePath + extractFileName(strPas(FFileHeaderDataEx.FileName));

        vExistentFile.Size := getFileSize(vExistentFile.FileName);
        vExistentFile.Time := getFileModifyDate(vExistentFile.FileName);

        if bRestoreFolder then
          vArchiveFile.FileName := strPas(FFileHeaderDataEx.FileName)
        else
          vArchiveFile.FileName := extractFileName(StrPas(FFileHeaderDataEx.FileName));

        vArchiveFile.Size := FFileHeaderDataEx.UnpSize;

        dosDateTimeToFileTime(hiWord(FFileHeaderDataEx.FileTime), loWord(FFileHeaderDataEx.FileTime), ft);
        fileTimeToSystemTime(ft, st);
        vArchiveFile.Time := systemTimeToDateTime(st);

        if fileExists(string(vExistentFile.FileName)) then
          if assigned(FOnReplace) then FOnReplace(SELF, vExistentFile, vArchiveFile, vReplaceResult);

        case vReplaceResult of
          rrCancel:     FAbort := TRUE;
          rrOverwrite:  if bRestoreFolder
                        then
                          vReadFileHeaderResult := RARProcessFile(FArchiveHandle, RAR_EXTRACT, PAnsiChar(aFilePath), NIL)
                        else
                        if (NOT ((FFileHeaderDataEx.Flags AND $00000070) = $00000070)) and (FFileHeaderDataEx.FileAttr <> faDirectory) then
                          vReadFileHeaderResult := RARProcessFile(FArchiveHandle, RAR_EXTRACT, Nil, PAnsiChar(vExistentFile.FileName));
          rrSkip:       begin
                          vReadFileHeaderResult := RARProcessFile(FArchiveHandle, RAR_SKIP, PAnsiChar(aFilePath), NIL);
                          {$WARN COMBINING_SIGNED_UNSIGNED OFF}
                          FProgressInfo.ArchiveBytesDone := FProgressInfo.ArchiveBytesDone + FFileHeaderDataEx.UnpSize;
                          {$WARN COMBINING_SIGNED_UNSIGNED ON}
                        end;
        end;

      end else vReadFileHeaderResult := RARProcessFile(FArchiveHandle, RAR_SKIP, NIL, NIL); // select next file without extracting

      if vReadFileHeaderResult <> RAR_SUCCESS then begin
        result := FALSE;
        checkRARResult(vReadFileHeaderResult, roListFiles);
      end;

      if FAbort then result := FALSE;
    end;
  finally
    closeArchive(FArchiveHandle);
  end;
  if FAbort then result := FALSE;
end;






end.
