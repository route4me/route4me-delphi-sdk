unit DirectionPathPointUnit;

interface

uses
  REST.Json.Types, SysUtils, System.Generics.Collections, Generics.Defaults,
  Math,
  DirectionLocationUnit;

type
  TDirectionPathPoint = class
  strict private
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

  TDirectionPathPointArray = TArray<TDirectionPathPoint>;

function SortDirectionPathPoints(DirectionPathPoints: TDirectionPathPointArray): TDirectionPathPointArray;

implementation

function SortDirectionPathPoints(DirectionPathPoints: TDirectionPathPointArray): TDirectionPathPointArray;
begin
  SetLength(Result, Length(DirectionPathPoints));
  TArray.Copy<TDirectionPathPoint>(DirectionPathPoints, Result, Length(DirectionPathPoints));
  TArray.Sort<TDirectionPathPoint>(Result, TComparer<TDirectionPathPoint>.Construct(
    function (const Direction1, Direction2: TDirectionPathPoint): Integer
    begin
      Result := CompareValue(Direction1.Latitude, Direction2.Latitude);
      if (Result = 0) then
        Result := CompareValue(Direction1.Longitude, Direction2.Longitude);
    end));
end;

{ TDirectionPathPoint }

end.
