unit DeviceLicenseResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit;

type
  TDeviceLicenseResponse = class(TGenericParameters)
  private
    [JSONName('status')]
    FStatus: boolean;

    [JSONName('routes_planned_this_month')]
    FRoutesPlannedThisMonth: integer;

    [JSONName('routes_remaining_this_month')]
    FRoutesRemainingThisMonth: integer;
  public
    property Status: boolean read FStatus write FStatus;
    property RoutesPlannedThisMonth: integer read FRoutesPlannedThisMonth;
    property RoutesRemainingThisMonth: integer read FRoutesRemainingThisMonth;
  end;

implementation

end.
