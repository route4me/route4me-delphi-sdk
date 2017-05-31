unit FileUploadingActionsUnit;

interface

uses
  SysUtils, Classes, BaseActionUnit,
  IConnectionUnit;

type
  TFileUploadingActions = class(TBaseAction)
  public
    function Preview(FileId: String; out ErrorString: String): TStringList;
    function UploadFileGeocoding(FileId: String; out ErrorString: String): TStringList;
    function Upload(Stream: TStream; out ErrorString: String): String;
  end;

implementation

uses
  System.Generics.Collections, GenericParametersUnit, CommonTypesUnit, SettingsUnit, UtilsUnit, FileUploadErrorsResponseUnit,
  FilePreviewErrorResponseUnit;

function TFileUploadingActions.Preview(FileId: String; out ErrorString: String): TStringList;
const
  ErrorTag = '{"status":false,"errors":["';
var
  Response: TObject;
  Parameters: TGenericParameters;
  ClassArray: TClassArray;
begin
  Result := TStringList.Create;

  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('strUploadID', FileId);
    Parameters.AddParameter('format', 'json');

    SetLength(ClassArray, 2);
    ClassArray[0] := TSimpleString;
    ClassArray[1] := TFilePreviewErrorResponse;
    Response := FConnection.Get(TSettings.EndPoints.CsvXlsPreview, Parameters,
      ClassArray, ErrorString);
    try
      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'File Preview fault';

      if (Response is TFilePreviewErrorResponse) then
      begin
        if Length(TFilePreviewErrorResponse(Response).Errors) > 0 then
          ErrorString := TFilePreviewErrorResponse(Response).Errors[0]
        else
          ErrorString := 'File Preview fault';
      end;

      if (Response is TSimpleString) and (ErrorString = EmptyStr) then
      begin
        if (pos(ErrorTag, TSimpleString(Response).Value) = 0) then
          Result.Text := TSimpleString(Response).Value
        else
          ErrorString := 'Error';

    (*todo: невозможно отличить строку {"status":false,"errors":["Upload not found"]} от контента
    Похоже парсинг этой строки надо производить здесь, а не в Connection
    *)
      end;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TFileUploadingActions.Upload(Stream: TStream;
  out ErrorString: String): String;
var
  Response: TSimpleString;
  Parameters: TGenericParameters;
begin
  Result := EmptyStr;

  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('format', 'json');

    Response := FConnection.Post(TSettings.EndPoints.FileUploading, Parameters,
      Stream, TSimpleString, ErrorString) as TSimpleString;
    try
      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'File Upload fault';

      Result := Response.Value;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

function TFileUploadingActions.UploadFileGeocoding(
  FileId: String; out ErrorString: String): TStringList;
var
  Response: TObject;
  Parameters: TGenericParameters;
  ClassArray: TClassArray;
begin
  Result := TStringList.Create;

  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('strUploadID', FileId);

    SetLength(ClassArray, 2);
    ClassArray[0] := TSimpleString;
    ClassArray[1] := TFileUploadErrorsResponse;
    Response := FConnection.Post(TSettings.EndPoints.CsvXlsGeocode, Parameters,
      ClassArray, ErrorString);
    try
      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Upload File Geocoding fault';

      if (Response is TFileUploadErrorsResponse) then
      begin
        if Length(TFileUploadErrorsResponse(Response).Errors) > 0 then
          ErrorString := TFileUploadErrorsResponse(Response).Errors[0]
        else
          ErrorString := 'Upload File Geocoding fault';
      end;

      if (Response is TSimpleString) and (ErrorString = EmptyStr) then
        Result.Text := TSimpleString(Response).Value;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
