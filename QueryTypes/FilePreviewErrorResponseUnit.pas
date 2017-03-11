unit FilePreviewErrorResponseUnit;

interface

uses
  REST.Json.Types, SysUtils,
  JSONNullableAttributeUnit,
  CommonTypesUnit;

type
  /// <summary>
  ///  Errors data-structure
  /// </summary>
  TFilePreviewErrorResponse = class
  private
    [JSONName('status')]
    FStatus: boolean;

    [JSONName('errors')]
    FErrors: TStringArray;
  public
    /// <remarks>
    ///  Constructor with 0-arguments must be and be public.
    ///  For JSON-deserialization.
    /// </remarks>
    constructor Create;
    destructor Destroy; override;

    property Status: boolean read FStatus write FStatus;

    /// <summary>
    /// Errors
    /// </summary>
    property Errors: TStringArray read FErrors write FErrors;
  end;

implementation

uses UtilsUnit;

constructor TFilePreviewErrorResponse.Create;
begin
  Inherited;

  SetLength(FErrors, 0);
end;

destructor TFilePreviewErrorResponse.Destroy;
begin
  Finalize(FErrors);
  inherited;
end;

end.
