unit UtilsUnit;

interface

uses
  System.Generics.Collections;
type
  TUtils = class
  public
    /// <summary>
    /// Convert DateTime to Unix epoch time
    /// </summary>
    class function ConvertToUnixTimestamp(Date: TDateTime): int64;
  end;

  TStringPair = TPair<String,String>;
  TArrayStringPair = TArray<TStringPair>;
  TListStringPair = TList<TStringPair>;

implementation

uses
  DateUtils;

{ TUtils }

class function TUtils.ConvertToUnixTimestamp(Date: TDateTime): int64;
begin
  Result := DateTimeToUnix(Date, False);
end;

end.
