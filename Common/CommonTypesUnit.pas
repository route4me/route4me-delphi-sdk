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

implementation

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

end.
