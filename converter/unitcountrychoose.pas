unit UnitCountryChoose;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst, Utils;

type

  { TFormCountryChoose }

  TFormCountryChoose = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    lstCountryTags: TCheckListBox;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    FIsUploaded: Boolean;
    FPath: string;
    FResultList: TChoosenCountries;
  end;

var
  FormCountryChoose: TFormCountryChoose;

implementation

uses
  FileUtil, RegExpr;

{$R *.lfm}

{ TFormCountryChoose }

procedure TFormCountryChoose.FormCreate(Sender: TObject);
begin
  FIsUploaded := false;
  FreeObject(FResultList);
  FResultList := TChoosenCountries.Create();
end;

procedure TFormCountryChoose.FormDestroy(Sender: TObject);
var
  i: Integer;
  ObjTmp: TObject;
begin
  for i := 0 to lstCountryTags.Count - 1 do
  begin
    ObjTmp := lstCountryTags.Items.Objects[i];
    FreeObject(ObjTmp);
  end;
  FreeObject(FResultList);
end;

procedure TFormCountryChoose.FormShow(Sender: TObject);
var
  i, j: Integer;
  SearchResult: TStringList;
  CountryTagFile: TStringList;
  RegExp: TRegExpr;
  country: string;
begin
  if (FIsUploaded OR (FPath = '')) then
    Exit;

  RegExp := TRegExpr.Create('^(\w{3})\s\=\s\"(.+)\"');
  RegExp.Compile();

  SearchResult := TStringList.Create();
  FileUtil.FindAllFiles(SearchResult, FPath + 'common\country_tags\', '*_countries.txt', False, 0);
  for i := 0 to SearchResult.Count - 1 do
  begin
    if (StringReplace(SearchResult[i], FPath + 'common\country_tags\', '', [rfIgnoreCase]) = 'zz_dynamic_countries.txt') then
      Continue;

    CountryTagFile := TStringList.Create();
    CountryTagFile.LoadFromFile(SearchResult[i]);
    for j := 0 to CountryTagFile.Count - 1 do
    begin
      if ((CountryTagFile[j] = '') OR (CountryTagFile[j][1] = '#')) then
        Continue;

      RegExp.Exec(CountryTagFile[j]);
      country := RegExp.Match[2] + ' (' + RegExp.Match[1] + ')';
      country := StringReplace(country, 'countries/', '', [rfReplaceAll]);
      country := StringReplace(country, '.txt', '', [rfReplaceAll]);
      lstCountryTags.AddItem(country, TStringHolder.Create(RegExp.Match[1])); // ADD to list
    end;
    FreeObject(CountryTagFile);
  end;
  FreeObject(SearchResult);
  lstCountryTags.Sorted := true;
  FormCountryChoose.Caption := 'Loaded ' + IntToStr(lstCountryTags.Count);
  FIsUploaded := true;
end;

procedure TFormCountryChoose.btnOkClick(Sender: TObject);
var
  i: Integer;
  ObjTmp: TStringHolder;
begin
  for i := 0 to lstCountryTags.Count - 1 do
    if (lstCountryTags.Checked[i]) then
    begin
      ObjTmp := TStringHolder(lstCountryTags.Items.Objects[i]);
      FResultList.Add(ObjTmp.FStr);
    end;
end;

end.

