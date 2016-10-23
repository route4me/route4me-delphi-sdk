unit DirectionLocationUnit;

interface

uses
  REST.Json.Types, SysUtils;

type
  TDirectionLocation = class
  private
    [JSONName('name')]
    FName: String;

    [JSONName('time')]
    FTime: integer;

    [JSONName('segment_distance')]
    FSegmentDistance: double;

    [JSONName('start_location')]
    FStartLocation: String;

    [JSONName('end_location')]
    FEndLocation: String;

    [JSONName('directions_error')]
    FDirectionsError: String;

    [JSONName('error_code')]
    FErrorCode: integer;
  public
    function Compare(Other: TDirectionLocation): integer;

    property Name: String read FName write FName;

    /// <summary>
    ///  Segment time (seconds)
    /// </summary>
    property Time: integer read FTime write FTime;

    property SegmentDistance: double read FSegmentDistance write FSegmentDistance;
    property StartLocation: String read FStartLocation write FStartLocation;
    property EndLocation: String read FEndLocation write FEndLocation;
    property DirectionsError: String read FDirectionsError write FDirectionsError;
    property ErrorCode: integer read FErrorCode write FErrorCode;
  end;

implementation

{ TDirectionLocation }

function TDirectionLocation.Compare(Other: TDirectionLocation): integer;
begin
  Result := String.Compare(Name, Other.Name);
end;

end.
