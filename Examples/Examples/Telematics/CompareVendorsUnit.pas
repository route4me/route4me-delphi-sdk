unit CompareVendorsUnit;

interface

uses SysUtils, Classes, BaseExampleUnit, EnumsUnit, CommonTypesUnit;

type
  TCompareVendors = class(TBaseExample)
  public
    procedure Execute(VendorIds: TStringArray);
  end;

implementation

uses NullableBasicTypesUnit, VendorUnit;

procedure TCompareVendors.Execute(VendorIds: TStringArray);
var
  ErrorString: String;
  Result: TVendorList;
begin
  Result := Route4MeManager.Telematics.Compare(VendorIds, ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
    begin
      WriteLn('CompareVendors successfully');
      WriteLn('');
    end
    else
      WriteLn(Format('CompareVendors error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Result);
  end;
end;

end.
