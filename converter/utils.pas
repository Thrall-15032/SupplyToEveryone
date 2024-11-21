unit Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fgl;

type
  IntArray = array of Integer;
  TChoosenCountries = specialize TFPGList<string>;
  TProvinceSet = specialize TFPGList<Integer>;
  TProvinceMap = specialize TFPGMap<TColor, Integer>;
  TNeighborMap = specialize TFPGMap<Integer, IntArray>;

  { TStringHolder }

  TStringHolder = class
  public
    FStr: string;
    constructor Create(AStr: string);
  end;

procedure FreeObject(AObj: TObject);
procedure CheckHoi4Game(AFolder: string);
procedure CheckSuppMod(AFolder: string);

var
  IsCountryTagsGame: Boolean;
  IsStatesGame: Boolean;
  IsDefinitionGame: Boolean;
  IsProvinceMapGame: Boolean;
  IsAdjacencyGame: Boolean;
  IsDescriptorMod: Boolean;

implementation

uses
  FileUtil;

{ TStringHolder }

constructor TStringHolder.Create(AStr: string);
begin
  FStr := AStr;
end;

procedure FreeObject(AObj: TObject);
begin
  if (Assigned(AObj)) then
    FreeAndNil(AObj);
end;

procedure CheckHoi4Game(AFolder: string);
var
  SearchResult: TStringList;
begin
  IsStatesGame := false;
  IsDefinitionGame := false;
  IsProvinceMapGame := false;
  IsAdjacencyGame := false;

  if DirectoryExists(AFolder + 'common\country_tags\') then
  begin
    SearchResult := TStringList.Create();
    FileUtil.FindAllFiles(SearchResult, AFolder + 'common\country_tags\', '*_countries.txt', False, 0);
    IsCountryTagsGame := SearchResult.Count > 0;
    FreeAndNil(SearchResult);
  end
  else
    IsCountryTagsGame := false;

  if DirectoryExists(AFolder + 'history\states\') then
  begin
    SearchResult := TStringList.Create();
    FileUtil.FindAllFiles(SearchResult, AFolder + 'history\states\', '*.txt', False, 0);
    IsStatesGame := SearchResult.Count > 0;
    FreeAndNil(SearchResult);
  end
  else
    IsStatesGame := false;

  if (DirectoryExists(AFolder + 'map\')) then
  begin
    IsDefinitionGame := FileExists(AFolder + 'map\definition.csv');
    IsProvinceMapGame := FileExists(AFolder + 'map\provinces.bmp');
    IsAdjacencyGame := FileExists(AFolder + 'map\adjacencies.csv');
  end
  else
  begin
    IsDefinitionGame := false;
    IsProvinceMapGame := false;
    IsAdjacencyGame := false;
  end;
end;

procedure CheckSuppMod(AFolder: string);
var
  i: Integer;
  StrList: TStringList;
  HasName, HasFileId: Boolean;
begin
  if (NOT(FileExists(AFolder + 'descriptor.mod'))) then
  begin
    IsDescriptorMod := false;
    Exit;
  end;
  StrList := TStringList.Create();
  StrList.LoadFromFile(AFolder + 'descriptor.mod');
  HasName := false;
  HasFileId := false;
  for i := 0 to StrList.Count - 1 do
  begin
    if (StrList[i] = 'name="Supply to Everyone"') then
      HasName := true;
    if (StrList[i] = 'remote_file_id="3327835518"') then
      HasFileId := true;
  end;
  FreeAndNIl(StrList);
  IsDescriptorMod := HasName AND HasFileId;
end;

end.

