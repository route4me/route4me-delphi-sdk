unit FileUploadingActionsUnit;

interface

uses
  SysUtils, Classes, BaseActionUnit,
  IConnectionUnit;

type
  TFileUploadingActions = class(TBaseAction)
  public
    function Preview(FileId: String; out ErrorString: String): String;
    function UploadFileGeocoding(FileId: String; out ErrorString: String): String;
    function Upload(Stream: TStream; out ErrorString: String): String;
  end;

implementation

uses
  System.Generics.Collections, GenericParametersUnit, CommonTypesUnit, SettingsUnit;

function TFileUploadingActions.Preview(FileId: String; out ErrorString: String): String;
var
  Response: TSimpleString;
  Parameters: TGenericParameters;
begin
  Result := EmptyStr;

  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('strUploadID', FileId);
    Parameters.AddParameter('format', 'json');

    Response := FConnection.Get(TSettings.EndPoints.CsvXlsPreview, Parameters,
      TSimpleString, ErrorString) as TSimpleString;
    try
      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'File Preview fault';

      Result := Response.Value;
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
  FileId: String; out ErrorString: String): String;
var
  Response: TSimpleString;
  Parameters: TGenericParameters;
begin
  Result := EmptyStr;

  Parameters := TGenericParameters.Create;
  try
    Parameters.AddParameter('strUploadID', FileId);

    Response := FConnection.Post(TSettings.EndPoints.CsvXlsGeocode, Parameters,
      TSimpleString, ErrorString) as TSimpleString;
    try
      if (Response = nil) and (ErrorString = EmptyStr) then
        ErrorString := 'Upload File Geocoding fault';

      Result := Response.Value;
    finally
      FreeAndNil(Response);
    end;
  finally
    FreeAndNil(Parameters);
  end;
end;

end.
