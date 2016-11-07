unit IAvoidanceZoneProviderUnit;

interface

uses
  AvoidanceZoneUnit;

type
  IAvoidanceZoneProvider = interface
    ['{E03EDA15-6146-4119-80BF-C18B1FE8E3BE}']

    function AvoidanceZones: TAvoidanceZoneList;
  end;

implementation

end.
