unit LinksUnit;

interface

uses
  REST.Json.Types, System.Generics.Collections, System.Rtti, Classes, SysUtils,
  JSONNullableAttributeUnit,
  NullableBasicTypesUnit;

type
  TLinks = class
  private
    [JSONName('route')]
    FRoute: String;

    [JSONName('view')]
    FView: String;

    [JSONName('optimization_problem_id')]
    FOptimizationProblemId: String;
  public
    function Equals(Obj: TObject): Boolean; override;

    property Route: String read FRoute write FRoute;
    property View: String read FView write FView;
    property OptimizationProblemId: String read FOptimizationProblemId write FOptimizationProblemId;
  end;

implementation

{ TLinks }

function TLinks.Equals(Obj: TObject): Boolean;
var
  Other: TLinks;
begin
  Result := False;

  if not (Obj is TLinks) then
    Exit;

  Other := TLinks(Obj);

  Result := (Route = Other.Route) and
    (View = Other.View) and
    (OptimizationProblemId = Other.OptimizationProblemId);
end;

end.
