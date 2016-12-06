unit AddressBookContactFindResponseUnit;

interface

uses
  REST.Json.Types,
  GenericParametersUnit, AddressNoteUnit, CommonTypesUnit;

type
  TAddressBookContactFindResponse = class(TGenericParameters)
  private
    [JSONName('results')]
    FResults: T2DimensionalStringArray;

    [JSONName('total')]
    FTotal: integer;

    [JSONName('fields')]
    FFields: TStringArray;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Results: T2DimensionalStringArray read FResults;
    property Total: integer read FTotal;
    property Fields: TStringArray read FFields;
  end;

implementation

constructor TAddressBookContactFindResponse.Create;
begin
  inherited;

  SetLength(FResults, 0);
  SetLength(FFields, 0);
end;

destructor TAddressBookContactFindResponse.Destroy;
begin
  Finalize(FResults);
  Finalize(FFields);

  inherited;
end;

end.
