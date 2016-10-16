unit JSONDictionaryInterceptorObjectUnit;

interface

uses
  System.Generics.Collections;

type
  TStringPair = TPair<String,String>;
  TDictionaryStringIntermediateObject = class
  private
    // This field can not be renamed
    FDictionaryIntermediateObject: TArray<TStringPair>;

    function GetPair(Key: String; List: TArray<TStringPair>; out Pair: TStringPair): boolean;
  public
    constructor Create;
    destructor Destroy; override;

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

    function Equals(Obj: TObject): Boolean; override;

    procedure Add(Key: String; Value: integer);
  end;

implementation

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
  PairEquals: boolean;
  Pair: TStringPair;
  OtherPair: TStringPair;
begin
  Result := False;

  if not (Obj is TDictionaryStringIntermediateObject) then
    Exit;

  Other := TDictionaryStringIntermediateObject(Obj);

  if (Length(FDictionaryIntermediateObject) <> Length(Other.FDictionaryIntermediateObject)) then
    Exit;

  PairEquals := True;
  for Pair in FDictionaryIntermediateObject do
  begin
    if not GetPair(Pair.Key, Other.FDictionaryIntermediateObject, OtherPair) then
      Exit;

    if (Pair.Value <> OtherPair.Value) then
      Exit;
  end;
  Result := True;
end;

function TDictionaryStringIntermediateObject.GetPair(Key: String;
  List: TArray<TStringPair>; out Pair: TStringPair): boolean;
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
  PairEquals: boolean;
  Pair: TIntegerPair;
  OtherPair: TIntegerPair;
begin
  Result := False;

  if not (Obj is TDictionaryIntegerIntermediateObject) then
    Exit;

  Other := TDictionaryIntegerIntermediateObject(Obj);

  if (Length(FDictionaryIntermediateObject) <> Length(Other.FDictionaryIntermediateObject)) then
    Exit;

  PairEquals := True;
  for Pair in FDictionaryIntermediateObject do
  begin
    if not GetPair(Pair.Key, Other.FDictionaryIntermediateObject, OtherPair) then
      Exit;

    if (Pair.Value <> OtherPair.Value) then
      Exit;
  end;
  Result := True;
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
