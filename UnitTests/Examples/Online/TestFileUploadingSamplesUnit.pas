unit TestFileUploadingSamplesUnit;

interface

uses
  TestFramework, Classes, SysUtils,
  BaseTestOnlineExamplesUnit;

type
  TTestFileUploadingSamples = class(TTestOnlineExamples)
  private
  published
    procedure Preview;
    procedure UploadFileGeocoding;
    procedure Upload;
  end;

implementation

uses
  VehicleUnit;

procedure TTestFileUploadingSamples.Preview;
var
  ErrorString: String;
  FileId: String;
  Content: String;
begin
  FileId := 'b2130b5fc36ae1109ef63b6db73781f6';
  Content := FRoute4MeManager.Uploading.Preview(FileId, ErrorString);
  CheckEquals(EmptyStr, ErrorString);
  CheckNotEquals(EmptyStr, Content);

  FileId := 'random_id_dDFsd2@D3d';
  Content := FRoute4MeManager.Uploading.Preview(FileId, ErrorString);
  // '{"status":false,"errors":["Upload not found"]}'
  CheckNotEquals(EmptyStr, ErrorString);
  CheckEquals(EmptyStr, Content);
end;

procedure TTestFileUploadingSamples.Upload;
var
  ErrorString: String;
  Content: String;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create('', fmOpenRead);
  try
    Content := FRoute4MeManager.Uploading.Upload(Stream, ErrorString);
  finally
    FreeAndNil(Stream);
  end;
  CheckEquals(EmptyStr, ErrorString);
  CheckNotEquals(EmptyStr, Content);

  Stream := TFileStream.Create('', fmOpenRead);
  try
    Content := FRoute4MeManager.Uploading.Upload(Stream, ErrorString);
  finally
    FreeAndNil(Stream);
  end;
  CheckNotEquals(EmptyStr, ErrorString);
  CheckEquals(EmptyStr, Content);
end;

procedure TTestFileUploadingSamples.UploadFileGeocoding;
var
  ErrorString: String;
  FileId: String;
  Content: TStringList;
begin
  FileId := 'b2130b5fc36ae1109ef63b6db73781f6';
  Content := FRoute4MeManager.Uploading.UploadFileGeocoding(FileId, ErrorString);
  // '{'#$A'    "optimization_problem_id": "F27753EA3D0734E854A35215B6712DA2",'#$A'    "address_count": 10,'#$A'    "status": true'#$A'}'
  CheckEquals(EmptyStr, ErrorString);
  CheckNotNull(Content);
  CheckTrue(Content.Count > 0);

  FileId := 'random_id_dDFsd2@D3d';
  Content := FRoute4MeManager.Uploading.UploadFileGeocoding(FileId, ErrorString);
  //'{"errors":["Upload not found"],"timestamp":1487595853}'
  CheckNotEquals(EmptyStr, ErrorString);
  CheckNotNull(Content);
  CheckTrue(Content.Count = 0);
end;

initialization
  RegisterTest('Examples\Online\FileUploading\', TTestFileUploadingSamples.Suite);
end.
