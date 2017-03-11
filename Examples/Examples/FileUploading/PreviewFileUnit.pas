unit PreviewFileUnit;

interface

uses SysUtils, Classes, BaseExampleUnit, EnumsUnit;

type
  TPreviewFile = class(TBaseExample)
  public
    procedure Execute(FileId: String);
  end;

implementation

uses NullableBasicTypesUnit;

procedure TPreviewFile.Execute(FileId: String);
var
  ErrorString: String;
  FileContent: TStringList;
begin
  FileContent := Route4MeManager.Uploading.Preview(FileId, ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
    begin
      WriteLn('File previewed successfully');
      WriteLn('');
    end
    else
      WriteLn(Format('FilePreview  error: "%s"', [ErrorString]));
  finally
    FreeAndNil(FileContent);
  end;
end;

end.
