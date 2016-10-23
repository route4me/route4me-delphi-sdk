unit CommonTypesUnit;

interface

uses
  System.Generics.Collections, Generics.Defaults, SysUtils;

type
  TStringArray = TArray<String>;

  function SortStringArray(Strings: TStringArray): TStringArray;

implementation

function SortStringArray(Strings: TStringArray): TStringArray;
begin
  SetLength(Result, Length(Strings));
  TArray.Copy<String>(Strings, Result, Length(Strings));
  TArray.Sort<String>(Result, TComparer<String>.Construct(
    function (const String1, String2: String): Integer
    begin
      Result := String.Compare(String1, String2);
    end));
end;

end.

