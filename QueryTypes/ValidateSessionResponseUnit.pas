unit ValidateSessionResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit;

type
  TValidateSessionResponse = class(TGenericParameters)
  private
    [JSONName('authenticated')]
    FAuthenticated: boolean;

    [JSONName('member_id')]
    FMemberId: integer;
  public
    property Authenticated: boolean read FAuthenticated write FAuthenticated;
    property MemberId: integer read FMemberId write FMemberId;
  end;

implementation

end.
