unit DirectionPathPointUnit;

interface

uses
  REST.Json.Types, DirectionLocationUnit;

type
  TDirectionPathPoint = class
  private
    [JSONName('lat')]
    FLatitude: double;

    [JSONName('lng')]
    FLongitude: double;
  public
    /// <summary>
    ///  Latitude
    /// </summary>
    property Latitude: double read FLatitude write FLatitude;
    /// <summary>
    ///  Longitude
    /// </summary>
    property Longitude: double read FLongitude write FLongitude;
  end;

implementation

{ TDirectionPathPoint }

end.
