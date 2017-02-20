unit UploadFileGeocodingUnit;

interface

uses SysUtils, BaseExampleUnit, EnumsUnit;

type
  TUploadFileGeocoding = class(TBaseExample)
  public
    procedure Execute(FileId: String);
  end;

implementation

uses NullableBasicTypesUnit;

procedure TUploadFileGeocoding.Execute(FileId: String);
var
  ErrorString: String;
  FileContent: String;
begin
  FileContent := Route4MeManager.Uploading.UploadFileGeocoding(FileId, ErrorString);

  WriteLn('');

  if (ErrorString = EmptyStr) then
  begin
    WriteLn('UploadFileGeocoding successfully');
    WriteLn('');
  end
  else
    WriteLn(Format('UploadFileGeocoding error: "%s"', [ErrorString]));
end;

end.
