unit RemoveUserRequestUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit;

type
  TRemoveUserRequest = class(TGenericParameters)
  private
    [JSONName('member_id')]
    FMemberId: integer;
  public
    constructor Create(MemberId: integer); reintroduce;
  end;

implementation

constructor TRemoveUserRequest.Create(MemberId: integer);
begin
  Inherited Create;

  FMemberId := MemberId;
end;

end.
