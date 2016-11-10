unit DeleteRouteResponseUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, SysUtils,
  GenericParametersUnit, DataObjectUnit, CommonTypesUnit;

type
  TDeleteRouteResponse = class(TGenericParameters)
  private
    [JSONName('deleted')]
    FDeleted: boolean;

    [JSONName('errors')]
    FErrors: TList<String>;

    [JSONName('route_id')]
    FRouteId: String;

    [JSONName('route_ids')]
    FRouteIds: TStringArray;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Deleted: boolean read FDeleted write FDeleted;
    property Errors: TList<String> read FErrors write FErrors;
    property RouteId: String read FRouteId write FRouteId;
    property RouteIds: TStringArray read FRouteIds write FRouteIds;
  end;

implementation

{ TDeleteRouteResponse }

constructor TDeleteRouteResponse.Create;
begin
  inherited;

  FErrors := nil;
  SetLength(FRouteIds, 0);
end;

destructor TDeleteRouteResponse.Destroy;
begin
  FreeAndNil(FErrors);

  inherited;
end;

end.
