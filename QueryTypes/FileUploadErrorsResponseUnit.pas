unit FileUploadErrorsResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, NullableBasicTypesUnit, CommonTypesUnit,
  JSONNullableAttributeUnit;

type
  TFileUploadErrorsResponse = class(TGenericParameters)
  private
    [JSONName('errors')]
    FErrors: TStringArray;

    [JSONName('timestamp')]
    [Nullable]
    FTimestamp: NullableInteger;
  public
    constructor Create; override;

    property Errors: TStringArray read FErrors write FErrors;
    property Timestamp: NullableInteger read FTimestamp write FTimestamp;
  end;

implementation

constructor TFileUploadErrorsResponse.Create;
begin
  inherited;
  SetLength(FErrors, 0);
  FTimestamp := NullableInteger.Null;
end;

end.
