unit ResequenceAllRoutesRequestUnit;

interface

uses
  HttpQueryMemberAttributeUnit, GenericParametersUnit, EnumsUnit;

type
  TResequenceAllRoutesRequest = class(TGenericParameters)
  private
    [HttpQueryMember('route_id')]
    FRouteId: String;

    [HttpQueryMember('disable_optimization')]
    FDisableOptimization: boolean;

    [HttpQueryMember('optimize')]
    FOptimize: String;
  public
    constructor Create(RouteId: String; DisableOptimization: boolean; Optimize: Toptimize); reintroduce;
  end;

implementation

{ TResequenceAllRoutesRequest }

constructor TResequenceAllRoutesRequest.Create(RouteId: String;
  DisableOptimization: boolean; Optimize: TOptimize);
begin
  Inherited Create;

  FRouteId := RouteId;
  FDisableOptimization := DisableOptimization;
  FOptimize := TOptimizeDescription[Optimize];
end;

end.
