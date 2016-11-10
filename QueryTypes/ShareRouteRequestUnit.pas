unit ShareRouteRequestUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit;

type
  TShareRouteRequest = class(TGenericParameters)
  private
    [JSONName('recipient_email')]
    FRecipientEmail: String;
  public
    constructor Create(RecipientEmail: String); reintroduce;
  end;

implementation

{ TShareRouteRequest }

constructor TShareRouteRequest.Create(RecipientEmail: String);
begin
  Inherited Create;
  FRecipientEmail := RecipientEmail;
end;

end.
