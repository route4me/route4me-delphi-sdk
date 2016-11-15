unit MergeRouteRequestUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, CommonTypesUnit;

type
  TMergeRouteRequest = class(TGenericParameters)
  private
    [JSONName('route_ids')]
    FRouteIds: TStringArray;
  public
    property RouteIds: TStringArray read FRouteIds write FRouteIds;
  end;

implementation

end.
