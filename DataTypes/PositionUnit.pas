unit PositionUnit;

interface

type
  TPosition = record
    Latitude: double;
    Longitude: double;

    constructor Create(Latitude, Longitude: double);
  end;

implementation

{ TPosition }

constructor TPosition.Create(Latitude, Longitude: double);
begin
  Self.Latitude := Latitude;
  Self.Longitude := Longitude;
end;

end.
