unit ErrorResponseUnit;

interface

uses
  REST.Json.Types, SysUtils,
  JSONNullableAttributeUnit,
  CommonTypesUnit;

type
  /// <summary>
  ///  Errors data-structure
  /// </summary>
  TErrorResponse = class
  private
    [JSONName('errors')]
    FErrors: TStringArray;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create;

    function Equals(Obj: TObject): Boolean; override;

    /// <summary>
    /// Errors
    /// </summary>
    property Errors: TStringArray read FErrors write FErrors;
  end;

implementation

{ TErrorResponse }

constructor TErrorResponse.Create;
begin
  Inherited;

  SetLength(FErrors, 0);
end;

function TErrorResponse.Equals(Obj: TObject): Boolean;
var
  Other: TErrorResponse;
  SortedErrors1, SortedErrors2: TStringArray;
  i: integer;
begin
  Result := False;

  if not (Obj is TErrorResponse) then
    Exit;

  Other := TErrorResponse(Obj);

  if (Length(FErrors) = Length(Other.FErrors)) then
  begin
    SortedErrors1 := SortStringArray(FErrors);
    SortedErrors2 := SortStringArray(Other.FErrors);
    for i := 0 to Length(SortedErrors1) - 1 do
      if not SortedErrors1[i].Equals(SortedErrors2[i]) then
        Exit;
  end;

  Result := True;
end;

end.
