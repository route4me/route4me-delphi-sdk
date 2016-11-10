unit AddAddressNoteResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, AddressNoteUnit;

type
  TAddAddressNoteResponse = class(TGenericParameters)
  private
    [JSONName('status')]
    FStatus: boolean;

    [JSONName('note')]
    FNote: TAddressNote;
  public
    property Status: boolean read FStatus write FStatus;
    property Note: TAddressNote read FNote write FNote;
  end;

implementation

end.
