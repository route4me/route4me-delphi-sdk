unit MoveDestinationToRouteResponseUnit;

interface

uses
  REST.Json.Types, HttpQueryMemberAttributeUnit,
  GenericParametersUnit;

type
  TMoveDestinationToRouteResponse = class(TGenericParameters)
  private
    [JSONName('success')]
    FSuccess: boolean;

    [JSONName('error')]
    FError: String;
  public
    property Success: boolean read FSuccess write FSuccess;
    property Error: String read FError write FError;
  end;

implementation

end.
