unit JSONDictionaryInterceptorObjectUnit;

interface

uses
  System.Generics.Collections;

type

  TDictionaryStringIntermediateObject = class
  private
    // This field can not be renamed
    FDictionaryIntermediateObject: TArray<TPair<String,String>>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Key: String; Value: String);
  end;

  TDictionaryIntegerIntermediateObject = class
  private
    // This field can not be renamed
    FDictionaryIntermediateObject: TArray<TPair<String,integer>>;
  public
    constructor Create;
    destructor Destroy; override;

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

end.
