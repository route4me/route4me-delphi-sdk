unit UtilsUnit;

interface

type
  TUtils = class
  public
    /// <summary>
    /// Convert DateTime to Unix epoch time
    /// </summary>
    class function ConvertToUnixTimestamp(Date: TDateTime): int64; static;
  end;

implementation

uses
  DateUtils;

{ TUtils }

class function TUtils.ConvertToUnixTimestamp(Date: TDateTime): int64;
begin
  Result := DateTimeToUnix(Date, False);
end;

end.
