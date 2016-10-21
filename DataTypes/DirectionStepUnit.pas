unit DirectionStepUnit;

interface

uses
  REST.Json.Types, DirectionLocationUnit, DirectionPathPointUnit;

type
  TDirectionStep = class
  private
    [JSONName('direction')]
    FDirection: String;

    [JSONName('directions')]
    FDirections: String;

    [JSONName('distance')]
    FDistance: double;

    [JSONName('distance_unit')]
    FDistanceUnit: String;

    [JSONName('maneuverType')]
    FManeuverType: String;

    [JSONName('compass_direction')]
    FCompassDirection: String;

    [JSONName('duration_sec')]
    FDurationSec: integer;

    [JSONName('maneuverPoint')]
    FManeuverPoint: TDirectionPathPoint;
  public
    constructor Create;

    /// <summary>
    ///  Name (detailed)
    /// </summary>
    property Direction: String read FDirection write FDirection;
    /// <summary>
    ///  Name (brief)
    /// </summary>
    property Directions: String read FDirections write FDirections;
    property Distance: double read FDistance write FDistance;
    property DistanceUnit: String read FDistanceUnit write FDistanceUnit;

    property ManeuverType: String read FManeuverType write FManeuverType;
    property CompassDirection: String read FCompassDirection write FCompassDirection;
    property DurationSec: integer read FDurationSec write FDurationSec;
    property ManeuverPoint: TDirectionPathPoint read FManeuverPoint write FManeuverPoint;
  end;

implementation

{ TDirectionStep }

constructor TDirectionStep.Create;
begin
  FManeuverPoint := nil;
end;

end.
