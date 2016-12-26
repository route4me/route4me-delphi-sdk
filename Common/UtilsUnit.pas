unit UtilsUnit;

interface

uses
  Windows, SysUtils, System.Generics.Collections, Generics.Defaults, CommonTypesUnit;

type
  TUtils = class
  public
    /// <summary>
    /// Convert DateTime to Unix epoch time
    /// </summary>
    class function ConvertToUnixTimestamp(Date: TDateTime): int64;
    class function FloatToStrDot(Value: Extended): String;
    class function StrToFloat(Value: String): double;
    class function SortStringArray(Strings: TStringArray): TStringArray;
    class function SortIntegerArray(Integers: TIntegerArray): TIntegerArray;
    class function EncodeURL(URL: String): String;
  end;

var
  DottedFormat: TFormatSettings;

implementation

uses
  DateUtils, System.NetEncoding, IdURI, Math;

{ TUtils }

class function TUtils.ConvertToUnixTimestamp(Date: TDateTime): int64;
begin
  Result := DateTimeToUnix(Date, False);
end;

class function TUtils.EncodeURL(URL: String): String;
begin
  Result := TIdURI.ParamsEncode(URL);
//  Result := TNetEncoding.URL.Encode(URL);
end;

class function TUtils.FloatToStrDot(Value: Extended): String;
begin
  Result := FloatToStr(Value, DottedFormat);
end;

class function TUtils.SortIntegerArray(Integers: TIntegerArray): TIntegerArray;
begin
  SetLength(Result, Length(Integers));
  if Length(Integers) = 0 then
    Exit;

  TArray.Copy<integer>(Integers, Result, Length(Integers));
  TArray.Sort<integer>(Result, TComparer<integer>.Construct(
    function (const Value1, Value2: integer): Integer
    begin
      Result := Math.CompareValue(Value1, Value2);
    end));
end;

class function TUtils.SortStringArray(Strings: TStringArray): TStringArray;
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

class function TUtils.StrToFloat(Value: String): double;
begin
  Result := SysUtils.StrToFloat(Value, DottedFormat);
end;

initialization
//  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, DottedFormat);
{$WARN SYMBOL_PLATFORM OFF}
  DottedFormat := TFormatSettings.Create(LOCALE_SYSTEM_DEFAULT);
  DottedFormat.DecimalSeparator := '.';
{$WARN SYMBOL_PLATFORM ON}
end.
