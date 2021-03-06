unit SingleDepotMultipleDriverNoTimeWindowTestDataProviderUnit;

interface

uses
  SysUtils,
  BaseOptimizationParametersProviderUnit, AddressUnit, RouteParametersUnit,
  OptimizationParametersUnit;

type
  TSingleDepotMultipleDriverNoTimeWindowTestDataProvider = class(TBaseOptimizationParametersProvider)
  protected
    function MakeAddresses(): TArray<TAddress>; override;
    function MakeRouteParameters(): TRouteParameters; override;
    /// <summary>
    ///  After response some fields are changed from request.
    /// </summary>
    procedure CorrectForResponse(OptimizationParameters: TOptimizationParameters); override;
  public

  end;

implementation

{ TSingleDepotMultipleDriverNoTimeWindowTestDataProvider }

uses
  DateUtils,
  EnumsUnit, UtilsUnit;

procedure TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.CorrectForResponse(
  OptimizationParameters: TOptimizationParameters);
begin
  inherited;

end;

function TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.MakeAddresses: TArray<TAddress>;
var
  FirstAddress: TAddress;
begin
  Result := TArray<TAddress>.Create();

  FirstAddress := TAddress.Create(
    '40 Mercer st, New York, NY', 40.7213583, -74.0013082, 0);
  //indicate that this is a departure stop
  // single depot routes can only have one departure depot
  FirstAddress.IsDepot := True;
  AddAddress(FirstAddress, Result);

  AddAddress(TAddress.Create('new york, ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('Manhatten Island NYC', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('503 W139 St, NY,NY', 40.7109062, -74.0091848, 0), Result);
  AddAddress(TAddress.Create('203 grand st, new york, ny', 40.7188990, -73.9967320, 0), Result);
  AddAddress(TAddress.Create('119 Church Street', 40.7137757, -74.0088238, 0), Result);
  AddAddress(TAddress.Create('new york ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('broadway street, new york', 40.7191551, -74.0020849, 0), Result);
  AddAddress(TAddress.Create('Ground Zero, Vesey-Liberty-Church-West Streets New York NY 10038', 40.7233126, -74.0116602, 0), Result);
  AddAddress(TAddress.Create('226 ilyssa way staten lsland ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('185 franklin st.', 40.7192099, -74.0097670, 0), Result);
  AddAddress(TAddress.Create('new york city,', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('11 e. broaway 11038', 40.7132060, -73.9974019, 0), Result);
  AddAddress(TAddress.Create('Brooklyn Bridge, NY', 40.7053804, -73.9962503, 0), Result);
  AddAddress(TAddress.Create('World Trade Center Site, NY', 40.7114980, -74.0122990, 0), Result);
  AddAddress(TAddress.Create('New York Stock Exchange, NY', 40.7074242, -74.0116342, 0), Result);
  AddAddress(TAddress.Create('Wall Street, NY', 40.7079825, -74.0079781, 0), Result);
  AddAddress(TAddress.Create('Trinity Church, NY', 40.7081426, -74.0120511, 0), Result);
  AddAddress(TAddress.Create('World Financial Center, NY', 40.7104750, -74.0154930, 0), Result);
  AddAddress(TAddress.Create('Federal Hall, NY', 40.7073034, -74.0102734, 0), Result);
  AddAddress(TAddress.Create('Flatiron Building, NY', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('South Street Seaport, NY', 40.7069210, -74.0036380, 0), Result);
  AddAddress(TAddress.Create('Rockefeller Center, NY', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('FAO Schwarz, NY', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('Woolworth Building, NY', 40.7123903, -74.0083309, 0), Result);
  AddAddress(TAddress.Create('Met Life Building, NY', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('SOHO\\/Tribeca, NY', 40.7185650, -74.0120170, 0), Result);
  AddAddress(TAddress.Create('Macy��Tަ-���T�-���T�s, NY', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('City Hall, NY, NY', 40.7127047, -74.0058663, 0), Result);
  AddAddress(TAddress.Create('Macy&amp;acirc;�-���T�-���T�s, NY', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('1452 potter blvd bayshore ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('55 Church St. New York, NY', 40.7112320, -74.0102680, 0), Result);
  AddAddress(TAddress.Create('55 Church St, New York, NY', 40.7112320, -74.0102680, 0), Result);
  AddAddress(TAddress.Create('79 woodlawn dr revena ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('135 main st revena ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('250 greenwich st, new york, ny', 40.7131590, -74.0118890, 0), Result);
  AddAddress(TAddress.Create('79 grand, new york, ny', 40.7216958, -74.0024352, 0), Result);
  AddAddress(TAddress.Create('World trade center\\u000a', 40.7116260, -74.0107140, 0), Result);
  AddAddress(TAddress.Create('World trade centern', 40.7132910, -74.0118350, 0), Result);
  AddAddress(TAddress.Create('391 broadway new york', 40.7183693, -74.0027800, 0), Result);
  AddAddress(TAddress.Create('Fletcher street', 40.7063954, -74.0056353, 0), Result);
  AddAddress(TAddress.Create('2 Plum LanenPlainview New York', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('50 Kennedy drivenPlainview New York', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('7 Crestwood DrivenPlainview New York', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('85 west street nyc', 40.7096460, -74.0146140, 0), Result);
  AddAddress(TAddress.Create('New York, New York', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('89 Reade St, New York City, New York 10013', 40.7142970, -74.0059660, 0), Result);
  AddAddress(TAddress.Create('100 white st', 40.7172477, -74.0014351, 0), Result);
  AddAddress(TAddress.Create('100 white st\\u000a33040', 40.7172477, -74.0014351, 0), Result);
  AddAddress(TAddress.Create('Canal st and mulberry', 40.7170880, -73.9986025, 0), Result);
  AddAddress(TAddress.Create('91-83 111st st\\u000aRichmond hills ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('122-09 liberty avenOzone park ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('80-16 101 avenOzone park ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('6302 woodhaven blvdnRego park ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('39-02 64th stnWoodside ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('New York City, NY,', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('Pine st', 40.7069754, -74.0089557, 0), Result);
  AddAddress(TAddress.Create('Wall st', 40.7079825, -74.0079781, 0), Result);
  AddAddress(TAddress.Create('32 avenue of the Americas, NY, NY', 40.7201140, -74.0050920, 0), Result);
  AddAddress(TAddress.Create('260 west broadway, NY, NY', 40.7206210, -74.0055670, 0), Result);
  AddAddress(TAddress.Create('Long island, ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('27 Carley ave\\u000aHuntington ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('17 west neck RdnHuntington ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('206 washington st', 40.7131577, -74.0126091, 0), Result);
  AddAddress(TAddress.Create('Cipriani new york', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('Byshnell Basin. NY', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('89 Reade St, New York, New York 10013', 40.7142970, -74.0059660, 0), Result);
  AddAddress(TAddress.Create('250 Greenwich St, New York, New York 10007', 40.7133000, -74.0120000, 0), Result);
  AddAddress(TAddress.Create('64 Bowery, New York, New York 10013', 40.7165540, -73.9962700, 0), Result);
  AddAddress(TAddress.Create('142-156 Mulberry St, New York, New York 10013', 40.7192764, -73.9973096, 0), Result);
  AddAddress(TAddress.Create('80 Spring St, New York, New York 10012', 40.7226590, -73.9981820, 0), Result);
  AddAddress(TAddress.Create('182 Duane street ny', 40.7170879, -74.0101210, 0), Result);
  AddAddress(TAddress.Create('182 Duane St, New York, New York 10013', 40.7170879, -74.0101210, 0), Result);
  AddAddress(TAddress.Create('462 broome street nyc', 40.7225800, -74.0008980, 0), Result);
  AddAddress(TAddress.Create('117 mercer street nyc', 40.7239679, -73.9991585, 0), Result);
  AddAddress(TAddress.Create('Lucca antiques\\u000a182 Duane St, New York, New York 10013', 40.7167516, -74.0087482, 0), Result);
  AddAddress(TAddress.Create('Room and board\\u000a105 Wooster street nyc', 40.7229097, -74.0021852, 0), Result);
  AddAddress(TAddress.Create('Lucca antiquesn182 Duane St, New York, New York 10013', 40.7167516, -74.0087482, 0), Result);
  AddAddress(TAddress.Create('Room and boardn105 Wooster street nyc', 40.7229097, -74.0021852, 0), Result);
  AddAddress(TAddress.Create('Lucca antiques 182 Duane st new York ny', 40.7170879, -74.0101210, 0), Result);
  AddAddress(TAddress.Create('Property\\u000a14 Wooster street nyc', 40.7229097, -74.0021852, 0), Result);
  AddAddress(TAddress.Create('101 Crosby street nyc', 40.7235730, -73.9969540, 0), Result);
  AddAddress(TAddress.Create('Room and board \\u000a105 Wooster street nyc', 40.7229097, -74.0021852, 0), Result);
  AddAddress(TAddress.Create('Propertyn14 Wooster street nyc', 40.7229097, -74.0021852, 0), Result);
  AddAddress(TAddress.Create('Room and board n105 Wooster street nyc', 40.7229097, -74.0021852, 0), Result);
  AddAddress(TAddress.Create('Mecox gardens\\u000a926 Lexington nyc', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('25 sybil&apos;s crossing Kent lakes, ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('10149 ASHDALE LANE\\u000967\\u000967393253\\u0009\\u0009\\u0009SANTEE\\u0009CA\\u000992071\\u0009\\u0009280501691\\u000967393253\\u0009IFI\\u0009280501691\\u000905-JUN-10\\u000967393253', 40.7143000, -74.0067000, 0), Result);
  AddAddress(TAddress.Create('193 Lakebridge Dr, Kings Paark, NY', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('219 west creek', 40.7198564, -74.0121098, 0), Result);
  AddAddress(TAddress.Create('14 North Moore Street\\u000aNew York, ny', 40.7196970, -74.0066100, 0), Result);
  AddAddress(TAddress.Create('14 North Moore StreetnNew York, ny', 40.7196970, -74.0066100, 0), Result);
  AddAddress(TAddress.Create('14 North Moore Street New York, ny', 40.7196970, -74.0066100, 0), Result);
  AddAddress(TAddress.Create('30-38 Fulton St, New York, New York 10038', 40.7077737, -74.0043299, 0), Result);
  AddAddress(TAddress.Create('73 Spring Street Ny NY', 40.7225378, -73.9976742, 0), Result);
  AddAddress(TAddress.Create('119 Mercer Street Ny NY', 40.7241390, -73.9993110, 0), Result);
  AddAddress(TAddress.Create('525 Broadway Ny NY', 40.7230410, -73.9991650, 0), Result);
  AddAddress(TAddress.Create('Church St', 40.7154338, -74.0075430, 0), Result);
  AddAddress(TAddress.Create('135 union stnWatertown ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('21101 coffeen stnWatertown ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('215 Washington stnWatertown ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('619 mill stnWatertown ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('3 canel st, new York, ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('new york city new york', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('50 grand street', 40.7225780, -74.0038019, 0), Result);
  AddAddress(TAddress.Create('Orient ferry, li ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('Hilton hotel river head li ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('116 park pl', 40.7140565, -74.0110155, 0), Result);
  AddAddress(TAddress.Create('long islans new york', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('1 prospect pointe niagra falls ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('New York City\\u0009NY', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('pink berry ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('New York City\\u0009 NY', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('10108', 40.7143000, -74.0067000, 0), Result);
  AddAddress(TAddress.Create('Ann st', 40.7105937, -74.0073715, 0), Result);
  AddAddress(TAddress.Create('Hok 620 ave of Americas new York ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('Som 14 wall st nyc', 40.7076179, -74.0107630, 0), Result);
  AddAddress(TAddress.Create('New York ,ny', 40.7142691, -74.0059729, 0), Result);
  AddAddress(TAddress.Create('52 prince st. 10012', 40.7235840, -73.9961170, 0), Result);
  AddAddress(TAddress.Create('451 broadway 10013', 40.7205177, -74.0009557, 0), Result);
  AddAddress(TAddress.Create('Dover street', 40.7087886, -74.0008644, 0), Result);
  AddAddress(TAddress.Create('Murray st', 40.7148929, -74.0113349, 0), Result);
  AddAddress(TAddress.Create('85 West St, New York, New York', 40.7096460, -74.0146140, 0), Result);
  AddAddress(TAddress.Create('NYC', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('64 trinity place, ny, ny', 40.7081649, -74.0127168, 0), Result);
  AddAddress(TAddress.Create('150 broadway ny ny', 40.7091850, -74.0100330, 0), Result);
  AddAddress(TAddress.Create('Pinegrove Dude Ranch 31 cherrytown Rd Kerhinkson Ny', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('Front street', 40.7063990, -74.0045493, 0), Result);
  AddAddress(TAddress.Create('234 canal St new York, NY 10013', 40.7177010, -73.9999570, 0), Result);
  AddAddress(TAddress.Create('72 spring street, new york ny 10012', 40.7225093, -73.9976540, 0), Result);
  AddAddress(TAddress.Create('150 spring street, new york, ny 10012', 40.7242393, -74.0014922, 0), Result);
  AddAddress(TAddress.Create('580 broadway street, new york, ny 10012', 40.7244210, -73.9970260, 0), Result);
  AddAddress(TAddress.Create('42 trinity place, new york, ny 10007', 40.7074000, -74.0135510, 0), Result);
  AddAddress(TAddress.Create('baco ny', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('Micro Tel Inn Alburn New York', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('20 Cedar Close', 40.7068734, -74.0078613, 0), Result);
  AddAddress(TAddress.Create('South street', 40.7080184, -73.9999414, 0), Result);
  AddAddress(TAddress.Create('47 Lafayette street', 40.7159204, -74.0027332, 0), Result);
  AddAddress(TAddress.Create('Newyork', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('Ground Zero, NY', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('GROUND ZERO NY', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('33400 SE Harrison', 40.7188400, -74.0103330, 0), Result);
  AddAddress(TAddress.Create('new york, new york', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('8 Greene St, New York, 10013', 40.7206160, -74.0027600, 0), Result);
  AddAddress(TAddress.Create('226 w 44st new york city', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('s street seaport 11 fulton st new york city', 40.7069150, -74.0033215, 0), Result);
  AddAddress(TAddress.Create('30 Rockefeller Plaza w 49th St New York City', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('30 Rockefeller Plaza 50th St New York City', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('S. Street Seaport 11 Fulton St. New York City', 40.7069150, -74.0033215, 0), Result);
  AddAddress(TAddress.Create('30 rockefeller plaza w 49th st, new york city', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('30 rockefeller plaza 50th st, new york city', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('11 fulton st, new york city', 40.7069150, -74.0033215, 0), Result);
  AddAddress(TAddress.Create('new york city ny', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('Big apple', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('Ny', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('New York new York', 40.7143528, -74.0059731, 0), Result);
  AddAddress(TAddress.Create('83-85 Chambers St, New York, New York 10007', 40.7148130, -74.0068890, 0), Result);
  AddAddress(TAddress.Create('New York', 40.7145502, -74.0071249, 0), Result);
  AddAddress(TAddress.Create('102 North End Ave NY, NY', 40.7147980, -74.0159690, 0), Result);
  AddAddress(TAddress.Create('57 Thompson St, New York, New York 10012', 40.7241400, -74.0035860, 0), Result);
  AddAddress(TAddress.Create('new york city', 40.7145500, -74.0071250, 0), Result);
  AddAddress(TAddress.Create('nyc, ny', 40.7145502, -74.0071249, 0), Result);
  AddAddress(TAddress.Create('New York NY', 40.7145500, -74.0071250, 0), Result);
  AddAddress(TAddress.Create('285 West Broadway New York, NY 10013', 40.7208750, -74.0046310, 0), Result);
  AddAddress(TAddress.Create('100 avenue of the americas New York, NY 10013', 40.7233120, -74.0043950, 0), Result);
  AddAddress(TAddress.Create('270 Lafeyette st New York, NY 10012', 40.7238790, -73.9965270, 0), Result);
  AddAddress(TAddress.Create('560 Broadway New York, NY 10012', 40.7238540, -73.9974980, 0), Result);
  AddAddress(TAddress.Create('42 Wooster St New York, NY 10013', 40.7223860, -74.0024220, 0), Result);
  AddAddress(TAddress.Create('42 Wooster StreetNew York, NY 10013-2230', 40.7223633, -74.0026240, 0), Result);
  AddAddress(TAddress.Create('504 Broadway, New York, NY 10012', 40.7221444, -73.9992714, 0), Result);
  AddAddress(TAddress.Create('426 Broome Street, New York, NY 10013', 40.7213295, -73.9987121, 0), Result);
  AddAddress(TAddress.Create('City hall, nyc', 40.7122066, -74.0055026, 0), Result);
  AddAddress(TAddress.Create('South street seaport, nyc', 40.7069501, -74.0030848, 0), Result);
  AddAddress(TAddress.Create('Ground zero, nyc', 40.7116410, -74.0122530, 0), Result);
  AddAddress(TAddress.Create('Ground zero', 40.7116410, -74.0122530, 0), Result);
  AddAddress(TAddress.Create('Mulberry and canal, NYC', 40.7170900, -73.9985900, 0), Result);
  AddAddress(TAddress.Create('World Trade Center, NYC', 40.7116670, -74.0125000, 0), Result);
  AddAddress(TAddress.Create('South Street Seaport', 40.7069501, -74.0030848, 0), Result);
  AddAddress(TAddress.Create('Wall Street and Nassau Street, NYC', 40.7071400, -74.0106900, 0), Result);
  AddAddress(TAddress.Create('Trinity Church, NYC', 40.7081269, -74.0125691, 0), Result);
  AddAddress(TAddress.Create('Federal Hall National Memorial', 40.7069515, -74.0101638, 0), Result);
  AddAddress(TAddress.Create('Little Italy, NYC', 40.7196920, -73.9977650, 0), Result);
  AddAddress(TAddress.Create('New York, NY', 40.7145500, -74.0071250, 0), Result);
  AddAddress(TAddress.Create('New York City, NY,', 40.7145500, -74.0071250, 0), Result);
  AddAddress(TAddress.Create('new york,ny', 40.7145500, -74.0071300, 0), Result);
  AddAddress(TAddress.Create('Odeon cinema', 40.7168300, -74.0080300, 0), Result);
  AddAddress(TAddress.Create('New York City', 40.7145500, -74.0071300, 0), Result);
  AddAddress(TAddress.Create('52 broadway, ny,ny 1004', 40.7065000, -74.0123000, 0), Result);
  AddAddress(TAddress.Create('52 broadway, ny,ny 10004', 40.7065000, -74.0123000, 0), Result);
  AddAddress(TAddress.Create('22 beaver st, ny,ny 10004', 40.7048200, -74.0121800, 0), Result);
  AddAddress(TAddress.Create('54 pine st,ny,ny 10005', 40.7068600, -74.0084900, 0), Result);
  AddAddress(TAddress.Create('114 liberty st, ny,ny 10006', 40.7097700, -74.0122000, 0), Result);
  AddAddress(TAddress.Create('215 canal st,ny,ny 10013', 40.7174700, -73.9989500, 0), Result);
  AddAddress(TAddress.Create('new york city ny', 40.7145500, -74.0071300, 0), Result);
  AddAddress(TAddress.Create('World Trade Center, New York, NY', 40.7116700, -74.0125000, 0), Result);
  AddAddress(TAddress.Create('Chinatown, New York, NY', 40.7159600, -73.9974100, 0), Result);
  AddAddress(TAddress.Create('101 murray street new york, ny', 40.7152600, -74.0125100, 0), Result);
  AddAddress(TAddress.Create('nyc', 40.7145500, -74.0071200, 0), Result);
  AddAddress(TAddress.Create('510 broadway new york', 40.7223400, -73.9990160, 0), Result);
  AddAddress(TAddress.Create('nyc', 40.7145502, -74.0071249, 0), Result);
  AddAddress(TAddress.Create('Little Italy', 40.7196920, -73.9977647, 0), Result);
  AddAddress(TAddress.Create('463 Broadway, New York, NY', 40.7210590, -74.0006880, 0), Result);
  AddAddress(TAddress.Create('222 West Broadway, New York, NY', 40.7193520, -74.0064170, 0), Result);
  AddAddress(TAddress.Create('270 Lafayette street new York new york', 40.7238790, -73.9965270, 0), Result);
  AddAddress(TAddress.Create('New York, NY USA', 40.7145500, -74.0071250, 0), Result);
  AddAddress(TAddress.Create('97 Kenmare Street, New York, NY 10012', 40.7214370, -73.9969110, 0), Result);
  AddAddress(TAddress.Create('19 Beekman St, New York, New York 10038', 40.7107540, -74.0062870, 0), Result);
  AddAddress(TAddress.Create('Soho', 40.7241404, -74.0020213, 0), Result);
  AddAddress(TAddress.Create('Bergen, New York', 40.7145500, -74.0071250, 0), Result);
  AddAddress(TAddress.Create('478 Broadway, NY, NY', 40.7213360, -73.9997710, 0), Result);
  AddAddress(TAddress.Create('555 broadway, ny, ny', 40.7238830, -73.9982960, 0), Result);
  AddAddress(TAddress.Create('375 West Broadway, NY, NY', 40.7235000, -74.0026020, 0), Result);
  AddAddress(TAddress.Create('35 howard st, NY, NY', 40.7195240, -74.0010300, 0), Result);
  AddAddress(TAddress.Create('Pier 17 NYC', 40.7063660, -74.0026890, 0), Result);
  AddAddress(TAddress.Create('120 Liberty St NYC', 40.7097740, -74.0124510, 0), Result);
  AddAddress(TAddress.Create('80 White Street, NY, NY', 40.7178340, -74.0020520, 0), Result);
  AddAddress(TAddress.Create('Manhattan, NY', 40.7144300, -74.0061000, 0), Result);
  AddAddress(TAddress.Create('22 read st, ny', 40.7142010, -74.0044910, 0), Result);
  AddAddress(TAddress.Create('130 Mulberry St, New York, NY 10013-5547', 40.7182880, -73.9977110, 0), Result);
  AddAddress(TAddress.Create('new york city, ny', 40.7145500, -74.0071250, 0), Result);
  AddAddress(TAddress.Create('10038', 40.7092119, -74.0033631, 0), Result);
  AddAddress(TAddress.Create('11 Wall St, New York, NY 10005-1905', 40.7072900, -74.0112010, 0), Result);
  AddAddress(TAddress.Create('89 Reade St, New York, New York 10007', 40.7134560, -74.0034990, 0), Result);
  AddAddress(TAddress.Create('265 Canal St, New York, NY 10013-6010', 40.7188850, -74.0009000, 0), Result);
  AddAddress(TAddress.Create('39 Broadway, New York, NY 10006-3003', 40.7133450, -73.9961320, 0), Result);
  AddAddress(TAddress.Create('25 beaver street new york ny', 40.7051110, -74.0120070, 0), Result);
  AddAddress(TAddress.Create('100 church street new york ny', 40.7130430, -74.0096370, 0), Result);
  AddAddress(TAddress.Create('69 Mercer St, New York, NY 10012-4440', 40.7226490, -74.0006100, 0), Result);
  AddAddress(TAddress.Create('111 Worth St, New York, NY 10013-4008', 40.7159210, -74.0034100, 0), Result);
  AddAddress(TAddress.Create('240-248 Broadway, New York, New York 10038', 40.7127690, -74.0076810, 0), Result);
  AddAddress(TAddress.Create('12 Maiden Ln, New York, NY 10038-4002', 40.7094460, -74.0095760, 0), Result);
  AddAddress(TAddress.Create('291 Broadway, New York, NY 10007-1814', 40.7150000, -74.0061340, 0), Result);
  AddAddress(TAddress.Create('55 Liberty St, New York, NY 10005-1003', 40.7088430, -74.0093840, 0), Result);
  AddAddress(TAddress.Create('Brooklyn Bridge, NY', 40.7063440, -73.9974390, 0), Result);
  AddAddress(TAddress.Create('wall street', 40.7063889, -74.0094444, 0), Result);
  AddAddress(TAddress.Create('south street seaport, ny', 40.7069501, -74.0030848, 0), Result);
  AddAddress(TAddress.Create('little italy, ny', 40.7196920, -73.9977647, 0), Result);
  AddAddress(TAddress.Create('47 Pine St, New York, NY 10005-1513', 40.7067340, -74.0089280, 0), Result);
  AddAddress(TAddress.Create('22 cortlandt street new york ny', 40.7100820, -74.0102510, 0), Result);
  AddAddress(TAddress.Create('105 reade street new york ny', 40.7156330, -74.0085220, 0), Result);
  AddAddress(TAddress.Create('2 lafayette street new york ny', 40.7140310, -74.0038910, 0), Result);
  AddAddress(TAddress.Create('53 crosby street new york ny', 40.7219770, -73.9982450, 0), Result);
  AddAddress(TAddress.Create('2 Lafayette St, New York, NY 10007-1307', 40.7140310, -74.0038910, 0), Result);
  AddAddress(TAddress.Create('105 Reade St, New York, NY 10013-3840', 40.7156330, -74.0085220, 0), Result);
  AddAddress(TAddress.Create('chinatown, ny', 40.7159556, -73.9974133, 0), Result);
  AddAddress(TAddress.Create('250 Broadway, New York, NY 10007-2516', 40.7130180, -74.0074700, 0), Result);
  AddAddress(TAddress.Create('156 William St, New York, NY 10038-2609', 40.7097970, -74.0055770, 0), Result);
  AddAddress(TAddress.Create('100 Church St, New York, NY 10007-2601', 40.7130430, -74.0096370, 0), Result);
  AddAddress(TAddress.Create('33 Beaver St, New York, NY 10004-2736', 40.7050980, -74.0117200, 0), Result);
end;

function TSingleDepotMultipleDriverNoTimeWindowTestDataProvider.MakeRouteParameters: TRouteParameters;
begin
  Result := TRouteParameters.Create();
  Result.AlgorithmType := TAlgorithmType.CVRP_TW_MD;
  Result.RouteName := 'Single Depot, Multiple Driver, No Time Window';
  Result.StoreRoute := False;
  Result.RouteDate := 53583232; //TUtils.ConvertToUnixTimestamp(IncDay(Now, 1));
  Result.RouteTime := 60 * 60 * 7;
  Result.RT := True;
  Result.RouteMaxDuration := 86400;
  Result.VehicleCapacity := '20';
  Result.VehicleMaxDistanceMI := '99999';
  Result.Parts := 4;
  Result.Optimize := TOptimize.Time;
  Result.DistanceUnit := TDistanceUnit.MI;
  Result.DeviceType := TDeviceType.Web;
  Result.TravelMode := TTravelMode.Driving;
  Result.Metric := TMetric.Geodesic;
end;

end.
