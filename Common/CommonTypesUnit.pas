unit CommonTypesUnit;

interface

uses
  System.Generics.Collections, Generics.Defaults, SysUtils;

type
  TStringArray = TArray<String>;
  TStringPair = TPair<String,String>;
  TArrayStringPair = TArray<TStringPair>;
  TListStringPair = TList<TStringPair>;
  TListString = TList<String>;

  function SortStringArray(Strings: TStringArray): TStringArray;
  function EncodeURL(URL: String): String;

implementation

uses
  System.NetEncoding, IdURI;

function SortStringArray(Strings: TStringArray): TStringArray;
begin
  SetLength(Result, Length(Strings));
  if Length(Strings) = 0 then
    Exit;

  TArray.Copy<String>(Strings, Result, Length(Strings));
  TArray.Sort<String>(Result, TComparer<String>.Construct(
    function (const String1, String2: String): Integer
    begin
      Result := String.Compare(String1, String2);
    end));
end;

function EncodeURL(URL: String): String;
begin
  Result := TIdURI.ParamsEncode(URL);
//  Result := TNetEncoding.URL.Encode(URL);

end;

end.
