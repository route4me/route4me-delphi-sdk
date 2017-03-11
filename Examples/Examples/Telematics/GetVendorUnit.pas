unit GetVendorUnit;

interface

uses SysUtils, Classes, BaseExampleUnit, EnumsUnit;

type
  TGetVendor = class(TBaseExample)
  public
    procedure Execute(VendorId: integer);
  end;

implementation

uses NullableBasicTypesUnit, VendorUnit;

procedure TGetVendor.Execute(VendorId: integer);
var
  ErrorString: String;
  Vendor: TVendor;
begin
  Vendor := Route4MeManager.Telematics.Get(VendorId, ErrorString);
  try
    WriteLn('');

    if (ErrorString = EmptyStr) then
    begin
      WriteLn('GetVendor successfully');
      WriteLn('');
    end
    else
      WriteLn(Format('GetVendor error: "%s"', [ErrorString]));
  finally
    FreeAndNil(Vendor);
  end;
end;

end.
