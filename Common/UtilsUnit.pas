unit UtilsUnit;

interface

uses
  Windows, SysUtils, System.Generics.Collections;

type
  TUtils = class
  public
    /// <summary>
    /// Convert DateTime to Unix epoch time
    /// </summary>
    class function ConvertToUnixTimestamp(Date: TDateTime): int64;
    class function FloatToStrDot(Value: Extended): String;
  end;

var
  DottedFormat: TFormatSettings;

implementation

uses
  DateUtils;

{ TUtils }

class function TUtils.ConvertToUnixTimestamp(Date: TDateTime): int64;
begin
  Result := DateTimeToUnix(Date, False);
end;

class function TUtils.FloatToStrDot(Value: Extended): String;
begin
  Result := FloatToStr(Value, DottedFormat);
end;

initialization
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, DottedFormat);
  DottedFormat.DecimalSeparator := '.';
end.
