unit PreviewFileUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

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
  FileContent: String;
begin
  FileContent := Route4MeManager.Uploading.Preview(FileId, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn('File previewed successfully');
    WriteLn('');
  end
  else
    WriteLn(Format('FilePreview  error: "%s"', [ErrorString]));
end;

end.
