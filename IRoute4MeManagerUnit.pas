unit IRoute4MeManagerUnit;

interface

uses
  IConnectionUnit,
  AddressBookContactActionsUnit, OptimizationActionsUnit, RouteActionsUnit,
  UserActionsUnit, AddressNoteActionsUnit, AddressActionsUnit,
  AvoidanceZoneActionsUnit, OrderActionsUnit, ActivityActionsUnit,
  TrackingActionsUnit, GeocodingActionsUnit, TerritoryActionsUnit,
  VehicleActionsUnit, FileUploadingActionsUnit;

type
  IRoute4MeManager = interface
    ['{2E31D4E6-C42A-4C9B-9ED5-445C3F6D6690}']
    function Optimization: TOptimizationActions;
    function Route: TRouteActions;
    function AddressBookContact: TAddressBookContactActions;
    function User: TUserActions;
    function AddressNote: TAddressNoteActions;
    function Address: TAddressActions;
    function AvoidanceZone: TAvoidanceZoneActions;
    function Geocoding: TGeocodingActions;
    function Order: TOrderActions;
    function ActivityFeed: TActivityActions;
    function Tracking: TTrackingActions;
    function Territory: TTerritoryActions;
    function Vehicle: TVehicleActions;
    function Uploading: TFileUploadingActions;

    procedure SetConnectionProxy(Host: String; Port: integer; Username, Password: String);
    function Connection: IConnection;

    procedure Clear;
  end;

implementation

end.
