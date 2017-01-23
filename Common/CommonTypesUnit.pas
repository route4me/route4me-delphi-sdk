unit CommonTypesUnit;

interface

uses
  System.Generics.Collections, SysUtils;

type
  TIntegerArray = TArray<integer>;
  TStringArray = TArray<String>;
  T2DimensionalStringArray = array of array of String;
  TStringPair = TPair<String,String>;
  TArrayStringPair = TArray<TStringPair>;
  TListStringPair = TList<TStringPair>;
  TListString = TList<String>;
  TClassArray = TArray<TClass>;

  TSimpleString = class
  private
    FValue: String;
  public
    constructor Create(Value: String);

    property Value: String read FValue;
  end;

  TSimpleInteger = class
  private
    FValue: integer;
  public
    constructor Create(Value: integer);

    property Value: integer read FValue;
  end;

function SortSimpleIntegerArray(Integers: TArray<TSimpleInteger>): TArray<TSimpleInteger>;

implementation

uses
  Generics.Defaults, Math;

{ TSimpleString }

constructor TSimpleString.Create(Value: String);
begin
  FValue := Value;
end;

{ TSimpleInteger }

constructor TSimpleInteger.Create(Value: integer);
begin
  FValue := Value;
end;

function SortSimpleIntegerArray(Integers: TArray<TSimpleInteger>): TArray<TSimpleInteger>;
var
  i: integer;
begin
  SetLength(Result, Length(Integers));
  if Length(Integers) = 0 then
    Exit;

  for i := Low(Integers) to High(Integers) do
    Result[i] := Integers[i];
  TArray.Sort<TSimpleInteger>(Result, TComparer<TSimpleInteger>.Construct(
    function (const Value1, Value2: TSimpleInteger): Integer
    begin
      Result := Math.CompareValue(Value1.FValue, Value2.FValue);
    end));
end;

end.
