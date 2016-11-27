unit AddNewUserResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit;

type
  TAddNewUserResponse = class(TGenericParameters)
  private
    [JSONName('member_id')]
    FMemberId: integer;
  public
    property MemberId: integer read FMemberId write FMemberId;
  end;

implementation

end.
