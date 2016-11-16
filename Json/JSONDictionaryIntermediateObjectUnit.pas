unit JSONDictionaryIntermediateObjectUnit;

interface

uses
  Classes, SysUtils, System.Generics.Collections, System.Rtti, REST.JsonReflect,
  CommonTypesUnit;

type
  TStringPair = TPair<String,String>;
  TDictionaryStringIntermediateObject = class
  private
    // This field can not be renamed
    FDictionaryIntermediateObject: TArrayStringPair;

    function GetPair(Key: String; List: TArrayStringPair; out Pair: TStringPair): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // This class method need for JSON unmarshaling
    class function FromJsonString(JsonString: String): TDictionaryStringIntermediateObject;

    function Equals(Obj: TObject): Boolean; override;

    procedure Add(Key: String; Value: String);
  end;

  TIntegerPair = TPair<String,Integer>;
  TDictionaryIntegerIntermediateObject = class
  private
    // This field can not be renamed
    FDictionaryIntermediateObject: TArray<TIntegerPair>;

    function GetPair(Key: String; List: TArray<TIntegerPair>; out Pair: TIntegerPair): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // This class method need for JSON unmarshaling
    class function FromJsonString(JsonString: String): TDictionaryIntegerIntermediateObject;

    function Equals(Obj: TObject): Boolean; override;

    procedure Add(Key: String; Value: integer);
  end;

implementation

function GetPairs(JsonString: String): TArrayStringPair;
var
  i, j: integer;
  sl: TStringList;
  Pair: String;
begin
  SetLength(Result, 0);

  JsonString := copy(JsonString, 2, Length(JsonString) - 2);
  sl := TStringList.Create;
  try
    ExtractStrings([','], [' '], PWideChar(JsonString), sl);
    for i := 0 to sl.Count - 1 do
    begin
      Pair := StringReplace(sl[i], '"', '', [rfReplaceAll]);
      j := pos(':', Pair);
      if (j = 0) then
        raise Exception.Create(Format('String %s is not correct JSON-string', [JsonString]));

      SetLength(Result, Length(Result) + 1);
      Result[High(Result)].Key := copy(Pair, 1, j-1);
      Result[High(Result)].Value := copy(Pair, j+1, Length(Pair) - j);
    end;
  finally
    FreeAndNil(sl);
  end;
end;

{ TNullableDictionaryStringIntermediateObject }

procedure TDictionaryStringIntermediateObject.Add(Key, Value: String);
begin
  SetLength(FDictionaryIntermediateObject, Length(FDictionaryIntermediateObject) + 1);
  FDictionaryIntermediateObject[High(FDictionaryIntermediateObject)].Key := Key;
  FDictionaryIntermediateObject[High(FDictionaryIntermediateObject)].Value := Value;
end;

constructor TDictionaryStringIntermediateObject.Create;
begin
  SetLength(FDictionaryIntermediateObject, 0);
end;

destructor TDictionaryStringIntermediateObject.Destroy;
begin
  Finalize(FDictionaryIntermediateObject);
  inherited;
end;

function TDictionaryStringIntermediateObject.Equals(Obj: TObject): Boolean;
var
  Other: TDictionaryStringIntermediateObject;
  Pair: TStringPair;
  OtherPair: TStringPair;
begin
  Result := False;

  if not (Obj is TDictionaryStringIntermediateObject) then
    Exit;

  Other := TDictionaryStringIntermediateObject(Obj);

  if (Length(FDictionaryIntermediateObject) <> Length(Other.FDictionaryIntermediateObject)) then
    Exit;

  for Pair in FDictionaryIntermediateObject do
  begin
    if not GetPair(Pair.Key, Other.FDictionaryIntermediateObject, OtherPair) then
      Exit;

    if (Pair.Value <> OtherPair.Value) then
      Exit;
  end;
  Result := True;
end;

class function TDictionaryStringIntermediateObject.FromJsonString(
  JsonString: String): TDictionaryStringIntermediateObject;
var
  Pair: TStringPair;
begin
  Result := TDictionaryStringIntermediateObject.Create;
  for Pair in GetPairs(JsonString) do
    Result.Add(Pair.Key, Pair.Value);
end;

function TDictionaryStringIntermediateObject.GetPair(Key: String;
  List: TArrayStringPair; out Pair: TStringPair): boolean;
var
  APair: TStringPair;
begin
  Result := False;
  for APair in List do
    if (APair.Key = Key) then
    begin
      Pair := APair;
      Exit(True);
    end;
end;

{ TNullableDictionaryIntegerIntermediateObject }

procedure TDictionaryIntegerIntermediateObject.Add(Key: String;
  Value: integer);
begin
  SetLength(FDictionaryIntermediateObject, Length(FDictionaryIntermediateObject) + 1);
  FDictionaryIntermediateObject[High(FDictionaryIntermediateObject)].Key := Key;
  FDictionaryIntermediateObject[High(FDictionaryIntermediateObject)].Value := Value;
end;

constructor TDictionaryIntegerIntermediateObject.Create;
begin
  SetLength(FDictionaryIntermediateObject, 0);
end;

destructor TDictionaryIntegerIntermediateObject.Destroy;
begin
  Finalize(FDictionaryIntermediateObject);
  inherited;
end;

function TDictionaryIntegerIntermediateObject.Equals(Obj: TObject): Boolean;
var
  Other: TDictionaryIntegerIntermediateObject;
  Pair: TIntegerPair;
  OtherPair: TIntegerPair;
begin
  Result := False;

  if not (Obj is TDictionaryIntegerIntermediateObject) then
    Exit;

  Other := TDictionaryIntegerIntermediateObject(Obj);

  if (Length(FDictionaryIntermediateObject) <> Length(Other.FDictionaryIntermediateObject)) then
    Exit;

  for Pair in FDictionaryIntermediateObject do
  begin
    if not GetPair(Pair.Key, Other.FDictionaryIntermediateObject, OtherPair) then
      Exit;

    if (Pair.Value <> OtherPair.Value) then
      Exit;
  end;
  Result := True;
end;

class function TDictionaryIntegerIntermediateObject.FromJsonString(
  JsonString: String): TDictionaryIntegerIntermediateObject;
var
  Pair: TStringPair;
begin
  Result := TDictionaryIntegerIntermediateObject.Create;
  for Pair in GetPairs(JsonString) do
    Result.Add(Pair.Key, StrToInt(Pair.Value));
end;

function TDictionaryIntegerIntermediateObject.GetPair(Key: String;
  List: TArray<TIntegerPair>; out Pair: TIntegerPair): boolean;
var
  APair: TIntegerPair;
begin
  Result := False;
  for APair in List do
    if (APair.Key = Key) then
    begin
      Pair := APair;
      Exit(True);
    end;
end;

end.
