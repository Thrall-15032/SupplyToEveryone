unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DialogRes, LCLType, ComCtrls, Buttons, fgl, Utils;

type
  { TForm1 }

  TForm1 = class(TForm)
    btnGameFolder: TBitBtn;
    btnModFolder: TBitBtn;
    btnBuildings: TButton;
    btnRailways: TButton;
    btnDetails: TButton;
    Button1: TButton;
    chkInfrastructure: TCheckBox;
    chkSupplyNode: TCheckBox;
    chkNavalBase: TCheckBox;
    chkCountyChoose: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    edtInfrastructure: TEdit;
    edtNavalBase: TEdit;
    edtRailways: TEdit;
    Image1: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    UpDown3: TUpDown;
    procedure btnGameFolderClick(Sender: TObject);
    procedure btnModFolderClick(Sender: TObject);
    procedure btnBuildingsClick(Sender: TObject);
    procedure btnRailwaysClick(Sender: TObject);
    procedure btnDetailsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure chkCountyChooseChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FChoosenCountries: TChoosenCountries;
    procedure CheckFolders();
    procedure SetAsSubMod();
    procedure ShowProgress(AText: string; AMax: Integer; APos: Integer);
    procedure ScanProvinces(var FullProvinceSet: TProvinceSet; var ChoosenProvinceSet: TProvinceSet);
    procedure ParseOneProvice(
      AFileName: string; var CountryTag: string;
      var InsertIndex: Integer; var ProvinceSet: TProvinceSet
    );
    procedure AddNeighbor(var NeighborMap: TNeighborMap; Key: Integer; Value: IntArray);
    procedure RemoveNeighbor(var NeighborMap: TNeighborMap; Key: Integer; Value: IntArray);
    function ArrayHasInt(Arr: IntArray; x: Integer): Boolean;
    function IntArrayJoin(Value: IntArray): string;
    function IsNaval(provinceList: TStringList; provinceNo: Integer): Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  FileUtil, Unit3, UnitCountryChoose, RegExpr;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  CheckFolders();
  FChoosenCountries := TChoosenCountries.Create();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeObject(FChoosenCountries);
end;

procedure TForm1.CheckFolders();
var
  AllRight: Boolean;
begin
  btnBuildings.Enabled := false;
  btnRailways.Enabled := false;
  btnDetails.Enabled := false;
  chkCountyChoose.Enabled := false;
  ShowProgress('Check folders', 2, 0);
  CheckHoi4Game(Edit1.Text);
  ShowProgress('Check folders', 2, 1);
  CheckSuppMod(Edit2.Text);
  ShowProgress('Check folders', 2, 2);
  AllRight := IsStatesGame AND IsDefinitionGame AND IsProvinceMapGame AND IsAdjacencyGame AND IsDescriptorMod;
  if (AllRight) then
  begin
    Image1.ImageIndex := 1;
    btnBuildings.Enabled := true;
    btnRailways.Enabled := true;
    btnDetails.Enabled := true;
    chkCountyChoose.Enabled := true;
  end
  else
  begin
    Image1.ImageIndex := 2;
    btnDetails.Enabled := true;
  end;
end;

procedure TForm1.SetAsSubMod();
var
  i: Integer;
  PrimePath, PrimeModName, SupplyModPath: string;
  SupplyDescriptor, ResultList, TempList: TStringList;
  RegExp: TRegExpr;
  IsMod, IsDescriptorCreated: Boolean;
begin
  PrimePath := Edit1.Text;
  SupplyModPath := Edit2.Text;
  IsMod := FileExists(PrimePath + 'descriptor.mod');
  PrimeModName := '';
  if (IsMod) then
  begin
    TempList := TStringList.Create();
    TempList.LoadFromFile(PrimePath + 'descriptor.mod');
    RegExp := TRegExpr.Create('^name\s*=\s*\"(.*)\"');
    RegExp.Compile();
    for i := 0 to TempList.Count - 1 do
    begin
      if (RegExp.Exec(TempList[i])) then
      begin
        PrimeModName := RegExp.Match[1];
        Break;
      end;
    end;
    FreeAndNil(RegExp);
    FreeAndNil(TempList);
    if (PrimeModName = '') then
    begin
      ShowMessage('Mod name not found, SupplyToEveryone can not work as submod');
      Exit;
    end;
  end;

  SupplyDescriptor := TStringList.Create();
  ResultList := TStringList.Create();
  SupplyDescriptor.LoadFromFile(SupplyModPath + 'descriptor.mod');
  IsDescriptorCreated := false;
  for i := 0 to SupplyDescriptor.Count - 1 do
  begin
    if (SupplyDescriptor[i].Contains('dependencies')) then
    begin
      if (IsMod) then
      begin
        ResultList.Add('dependencies={"' + PrimeModName + '"}');
        IsDescriptorCreated := true;
      end;
      Continue;
    end;
    ResultList.Add(SupplyDescriptor[i]);
  end;
  if (IsMod AND (NOT(IsDescriptorCreated))) then
  begin
    ResultList.Add('dependencies={"' + PrimeModName + '"}');
  end;
  ResultList.SaveToFile(SupplyModPath + 'descriptor.mod');
  FreeAndNil(ResultList);
  FreeAndNil(SupplyDescriptor);
end;

procedure TForm1.btnGameFolderClick(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName := Edit1.Text;
  if (SelectDirectoryDialog1.Execute()) then
    Edit1.Text := SelectDirectoryDialog1.FileName + '\';
  CheckFolders();
end;

procedure TForm1.btnModFolderClick(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName := Edit2.Text;
  if (SelectDirectoryDialog1.Execute()) then
    Edit2.Text := SelectDirectoryDialog1.FileName + '\';
  CheckFolders();
end;

procedure TForm1.chkCountyChooseChange(Sender: TObject);
begin
  FChoosenCountries.Clear();
  if (NOT(chkCountyChoose.Checked)) then
  begin
    chkCountyChoose.Caption := 'Only choosen countries (0)';
    Exit;
  end;

  FormCountryChoose := TFormCountryChoose.Create(Form1);
  FormCountryChoose.FIsUploaded := false;
  FormCountryChoose.FPath := Edit1.Text;
  if ((FormCountryChoose.ShowModal = mrOK)
    AND (FormCountryChoose.FResultList.Count > 0)) then
  begin
    FChoosenCountries.Assign(FormCountryChoose.FResultList);
  end
  else
    chkCountyChoose.Checked := false;
  chkCountyChoose.Caption := 'Only choosen countries (' +
    IntToStr(FChoosenCountries.Count) + ')';
  FreeObject(FormCountryChoose);
end;

procedure TForm1.btnBuildingsClick(Sender: TObject);
var
  StatesPath, ResultPath, FileName, CountryTag: string;
  ProvinceDataList, StatesList, StateDataList: TStringList;
  ProvinceSet: TProvinceSet;
  i, j, InsertIndex: Integer;
  IsNavalProv: Boolean;
begin
  btnBuildings.Enabled := false;
  btnRailways.Enabled := false;
  SetAsSubMod();

  StatesPath := Edit1.Text + 'history\states\';
  ProvinceDataList := TStringList.Create();
  ProvinceDataList.LoadFromFile(Edit1.Text + 'map\definition.csv');
  StatesList := TStringList.Create();
  FileUtil.FindAllFiles(StatesList, StatesPath, '*.txt', False, 0);

  ResultPath := Edit2.Text + 'history\states\';
  if (NOT(DirectoryExists(ResultPath))) then
  begin
    if (NOT(DirectoryExists(Edit2.Text + 'history\'))) then
      CreateDir(Edit2.Text + 'history\');
    CreateDir(ResultPath);
  end
  else
    DeleteDirectory(ResultPath, true);

  ProvinceSet := TProvinceSet.Create();
  StateDataList := TStringList.Create();
  for i := 0 to statesList.Count - 1 do
  begin
    ProvinceSet.Clear();
    InsertIndex := -1;
    CountryTag := '';
    ParseOneProvice(StatesList[i], CountryTag, InsertIndex, ProvinceSet);
    // ProvinceSet.Sort();
    if ((InsertIndex = -1) AND (CountryTag = '')) then
      Continue;

    StateDataList.Clear();
    StateDataList.LoadFromFile(StatesList[i]);
    FileName := StringReplace(StatesList[i], StatesPath, '', [rfIgnoreCase]);

    if ((chkCountyChoose.Checked) AND (FChoosenCountries.IndexOf(CountryTag) < 0)) then
    begin
      // don't save file for country, game will use original data
      ShowProgress('States updating', statesList.Count - 1, i);
      Continue;
    end;

    // insert new buildings={...} param
    StateDataList.Insert(InsertIndex, #9#9 + 'buildings={');
    Inc(InsertIndex);
    if (chkInfrastructure.Checked) then
    begin
      StateDataList.Insert(InsertIndex, #9#9#9 + 'infrastructure=' + edtInfrastructure.Text);
      Inc(InsertIndex);
    end;
    for j := 0 to ProvinceSet.Count - 1 do
    begin
      IsNavalProv := IsNaval(ProvinceDataList, ProvinceSet.Items[j]);
      if (
        (NOT(chkSupplyNode.Checked) AND NOT(chkNavalBase.Checked))
        OR (NOT(chkSupplyNode.Checked) AND (chkNavalBase.Checked) AND (NOT(IsNavalProv)))) then
      begin
        Continue;
      end;

      StateDataList.Insert(InsertIndex, #9#9#9 + IntToStr(ProvinceSet.Items[j]) + '={');
      Inc(InsertIndex);
      if (chkSupplyNode.Checked) then
      begin
        StateDataList.Insert(InsertIndex, #9#9#9#9 + 'supply_node=1');
        Inc(InsertIndex);
      end;
      if (chkNavalBase.Checked AND IsNavalProv) then
      begin
        StateDataList.Insert(InsertIndex, #9#9#9#9 + 'naval_base=' + edtNavalBase.Text);
        Inc(InsertIndex);
      end;
      StateDataList.Insert(InsertIndex, #9#9#9 + '}');
      Inc(InsertIndex);
    end;
    StateDataList.Insert(InsertIndex, #9#9 + '}');
    Inc(InsertIndex);

    StateDataList.SaveToFile(ResultPath + FileName);
    ShowProgress('States updating', statesList.Count - 1, i);
  end;

  FreeAndNil(ProvinceSet);
  FreeAndNil(StatesList);
  FreeAndNil(ProvinceDataList);

  btnBuildings.Enabled := true;
  btnRailways.Enabled := true;
  ShowMessage('done');
end;

procedure TForm1.btnRailwaysClick(Sender: TObject);
var
  TmpList, ResList, ProvinceDataList, AdjacencyList, TrainwayList, ErrorList: TStringList;
  TmpStr, ResultPath: string;
  ProvinceSet, ChoosenProvinceSet: TProvinceSet;
  ProvinceMap: TProvinceMap;
  WorldBmp: TBitmap;
  NeighborMap: TNeighborMap;
  i, j, index, indexN, CurProvince: Integer;
  ArrN: IntArray;
begin
  btnBuildings.Enabled := false;
  btnRailways.Enabled := false;
  SetAsSubMod();

  // File definition.csv to Map<ProvinceColor, ProvinceId>
  ProvinceDataList := TStringList.Create();
  ProvinceDataList.LoadFromFile(Edit1.Text + 'map\definition.csv');
  ProvinceMap := TProvinceMap.Create();
  NeighborMap := TNeighborMap.Create();

  ResultPath := Edit2.Text + 'map\';
  if (NOT(DirectoryExists(ResultPath))) then
    CreateDir(ResultPath)
  else
    DeleteDirectory(ResultPath, true);

  TmpList := TStringList.Create();
  TmpList.Delimiter := ';';
  for i := 0 to ProvinceDataList.Count - 1 do
  begin
    if (ProvinceDataList[i] = '') then
      Continue;
    TmpList.Clear();
    TmpList.DelimitedText := ProvinceDataList[i];
    // only Land province, if you need all province (include sea and lake) remove that if()
    if (TmpList[4] <> 'land') then
      Continue;
    //
    ProvinceMap.Add(
      RGBToColor(StrToInt(TmpList[1]), StrToInt(TmpList[2]), StrToInt(TmpList[3])),
      StrToInt(TmpList[0])
    );
  end;
  FreeAndNil(TmpList);

  // Loop over world map, save result to Map<ProvinceId, NeighborIds[]>
  WorldBmp := TBitmap.Create();
  WorldBmp.LoadFromFile(Edit1.Text + 'map\provinces.bmp');
  for i := 0 to WorldBmp.Width - 1 do
  begin
    for j := 0 to WorldBmp.Height - 1 do
    begin
      index := ProvinceMap.IndexOf(WorldBmp.Canvas.Pixels[i, j]);
      if (index >= 0) then // not a sea
      begin
        CurProvince := ProvinceMap.Data[index];
        SetLength(ArrN, 0);
        if (i > 0) AND (WorldBmp.Canvas.Pixels[i - 1, j] <> WorldBmp.Canvas.Pixels[i, j]) then
        begin
          indexN := ProvinceMap.IndexOf(WorldBmp.Canvas.Pixels[i - 1, j]);
          if (indexN >= 0) then
          begin
            SetLength(ArrN, Length(ArrN) + 1);
            ArrN[Length(ArrN) - 1] := ProvinceMap.Data[indexN];
          end;
        end;
        if (j > 0) AND (WorldBmp.Canvas.Pixels[i, j - 1] <> WorldBmp.Canvas.Pixels[i, j]) then
        begin
          indexN := ProvinceMap.IndexOf(WorldBmp.Canvas.Pixels[i, j - 1]);
          if (indexN >= 0) then
          begin
            SetLength(ArrN, Length(ArrN) + 1);
            ArrN[Length(ArrN) - 1] := ProvinceMap.Data[indexN];
          end;
        end;
        if (i < WorldBmp.Width - 1) AND (WorldBmp.Canvas.Pixels[i + 1, j] <> WorldBmp.Canvas.Pixels[i, j]) then
        begin
          indexN := ProvinceMap.IndexOf(WorldBmp.Canvas.Pixels[i + 1, j]);
          if (indexN >= 0) then
          begin
            SetLength(ArrN, Length(ArrN) + 1);
            ArrN[Length(ArrN) - 1] := ProvinceMap.Data[indexN];
          end;
        end;
        if (j < WorldBmp.Height - 1) AND (WorldBmp.Canvas.Pixels[i, j + 1] <> WorldBmp.Canvas.Pixels[i, j]) then
        begin
          indexN := ProvinceMap.IndexOf(WorldBmp.Canvas.Pixels[i, j + 1]);
          if (indexN >= 0) then
          begin
            SetLength(ArrN, Length(ArrN) + 1);
            ArrN[Length(ArrN) - 1] := ProvinceMap.Data[indexN];
          end;
        end;
        AddNeighbor(NeighborMap, CurProvince, ArrN);
      end;
    end;
    ShowProgress('Scan province map', WorldBmp.Width - 1, i);
  end;

  // Apply data from file adjacencies.csv to Map<ProvinceId, NeighborIds[]>
  ProvinceSet := TProvinceSet.Create();
  ChoosenProvinceSet := TProvinceSet.Create();
  ScanProvinces(ProvinceSet, ChoosenProvinceSet);
  AdjacencyList := TStringList.Create();
  AdjacencyList.LoadFromFile(Edit1.Text + 'map\adjacencies.csv');
  ErrorList := TStringList.Create();
  TmpList := TStringList.Create();
  for i := 0 to AdjacencyList.Count - 1 do
  begin
    TmpList.Clear();
    if (AdjacencyList[i].Trim() = '') then
      Continue;
    TmpList.Delimiter := ';';
    TmpList.DelimitedText := AdjacencyList[i];
    if ((TmpList[0] = 'From') OR (TmpList[0] = '-1')) then
      Continue;
    if ((ProvinceSet.IndexOf(StrToInt(TmpList[0])) < 0)
      OR (ProvinceSet.IndexOf(StrToInt(TmpList[1])) < 0)) then
    begin
      ErrorList.Add(AdjacencyList[i]);
      Continue;
    end;

    if (TmpList[2] = 'sea') then
    begin
      AddNeighbor(NeighborMap, StrToInt(TmpList[0]), [StrToInt(TmpList[1])]);
      AddNeighbor(NeighborMap, StrToInt(TmpList[1]), [StrToInt(TmpList[0])]);
    end;
    if (TmpList[2] = 'impassable') then
    begin
      RemoveNeighbor(NeighborMap, StrToInt(TmpList[0]), [StrToInt(TmpList[1])]);
      RemoveNeighbor(NeighborMap, StrToInt(TmpList[1]), [StrToInt(TmpList[0])]);
    end;
    ShowProgress('Province update', AdjacencyList.Count - 1, i);
  end;
  ErrorList.SaveToFile(ResultPath + 'error_adjacencies.txt');
  FreeAndNil(TmpList);
  FreeAndNil(ProvinceSet);

  // Map<ProvinceId, NeighborIds[]> to string "ProvinceId Neigh1 Neigh2 NeighN"
  // Map<ProvinceId, NeighborIds[]> to train data "5 2 ProvinceId1 ProvinceId12"
  NeighborMap.Sort();
  ResList := TStringList.Create();
  TrainwayList := TStringList.Create();
  if (chkCountyChoose.Checked) then
  begin
    TrainwayList.LoadFromFile(Edit1.Text + 'map\railways.txt');
  end;
  for i := 0 to NeighborMap.Count - 1 do
  begin
    TmpStr := IntToStr(NeighborMap.Keys[i]) + ' ' + IntArrayJoin(NeighborMap.Data[i]);
    ResList.Add(TmpStr.Trim());
    if (NOT(Assigned(NeighborMap.Data[i]))) then
      Continue;
    for j := 0 to Length(NeighborMap.Data[i]) - 1 do
    begin
      if (NeighborMap.Keys[i] < NeighborMap.Data[i][j]) then
      begin
        if (chkCountyChoose.Checked) then
        begin
          if (ChoosenProvinceSet.IndexOf(NeighborMap.Keys[i]) >= 0) then
          begin
            TrainwayList.Add(edtRailways.Text + ' 2 ' + IntToStr(NeighborMap.Keys[i])
              + ' ' + IntToStr(NeighborMap.Data[i][j]));
          end;
        end
        else
        begin
          TrainwayList.Add(edtRailways.Text + ' 2 ' + IntToStr(NeighborMap.Keys[i])
            + ' ' + IntToStr(NeighborMap.Data[i][j]));
        end;
      end;
    end;
    ShowProgress('TrainWays creating', NeighborMap.Count - 1, i);
  end;

  ResList.SaveToFile(ResultPath + 'neighbor.txt');
  TrainwayList.SaveToFile(ResultPath + 'railways.txt');
  FreeAndNil(ChoosenProvinceSet);
  FreeAndNil(TrainwayList);
  FreeAndNil(ResList);
  FreeAndNil(WorldBmp);
  FreeAndNil(NeighborMap);
  FreeAndNil(ProvinceMap);
  FreeAndNil(ProvinceDataList);

  btnBuildings.Enabled := true;
  btnRailways.Enabled := true;
  ShowMessage('done');
end;

procedure TForm1.btnDetailsClick(Sender: TObject);
begin
  Form2 := TForm2.Create(Form1);
  Form2.ShowModal();
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  RegExp: TRegExpr;
  ts, tre: string;
  InsertIndex: Integer;
begin
  ts := '	}history={';
  if (ts.Contains('history')) then
  begin
    Showmessage(ts);
    InsertIndex := ts.IndexOf('history');
    ShowMessage(IntToStr(InsertIndex));
    ts := Copy(ts, InsertIndex + 1);
  end;

  ShowMessage(ts);
end;

procedure TForm1.ShowProgress(AText: string; AMax: Integer; APos: Integer);
var
  Percent: Integer;
  PercentStr: string;
begin
  Percent := (APos * 1000) div AMax;
  if (Percent = 0) then
    PercentStr := '0,0'
  else
  begin
    PercentStr := IntToStr(Percent);
    PercentStr := LeftStr(PercentStr, Length(PercentStr) - 1) + ',' + RightStr(PercentStr, 1);
  end;
  Panel1.Caption := AText + ' ' + PercentStr + '%';
  Application.ProcessMessages();
end;

procedure TForm1.ScanProvinces(
  var FullProvinceSet: TProvinceSet; var ChoosenProvinceSet: TProvinceSet);
var
  StatesPath, CountryTag: string;
  StatesList: TStringList;
  TempSet: TProvinceSet;
  i, j, Index: Integer;
begin
  StatesPath := Edit1.Text + 'history\states\';
  StatesList := TStringList.Create();
  FileUtil.FindAllFiles(StatesList, StatesPath, '*.txt', False, 0);

  TempSet := TProvinceSet.Create();
  for i := 0 to StatesList.Count - 1 do
  begin
    TempSet.Clear();
    Index := -1;
    CountryTag := '';
    ParseOneProvice(StatesList[i], CountryTag, Index, TempSet);
    for j := 0 to TempSet.Count - 1 do
    begin
      FullProvinceSet.Add(TempSet.Items[j]);
      if ((chkCountyChoose.Checked) AND (FChoosenCountries.IndexOf(CountryTag) >= 0)) then
      begin
        ChoosenProvinceSet.Add(TempSet.Items[j]);
      end;
    end;
    ShowProgress('Scan province data', statesList.Count - 1, i);
  end;
  FreeAndNil(TempSet);
  FreeAndNil(StatesList);
end;

procedure TForm1.ParseOneProvice(
  AFileName: string; var CountryTag: string;
  var InsertIndex: Integer; var ProvinceSet: TProvinceSet);
var
  TmpStr: string;
  StateDataList, ProvinceNoList: TStringList;
  RegExp: TRegExpr;
  i, index: Integer;
begin
  StateDataList := TStringList.Create();
  StateDataList.LoadFromFile(AFileName);
  index := StateDataList.Text.IndexOf('provinces');
  TmpStr := Copy(StateDataList.Text, index, Length(StateDataList.Text));
  index := TmpStr.IndexOf('}');
  TmpStr := Copy(TmpStr, 1, index + 1);
  RegExp := TRegExpr.Create('\#.*?\r?\n');
  RegExp.Compile();
  TmpStr := RegExp.Replace(TmpStr, ''); // Remove comments
  TmpStr := StringReplace(TmpStr, 'provinces', '', [rfReplaceAll]);
  TmpStr := StringReplace(TmpStr, '=', '', [rfReplaceAll]);
  TmpStr := StringReplace(TmpStr, '{', '', [rfReplaceAll]);
  TmpStr := StringReplace(TmpStr, '}', '', [rfReplaceAll]);
  ProvinceNoList := TStringList.Create();
  ProvinceNoList.DelimitedText := TmpStr;
  for i := 0 to ProvinceNoList.Count - 1 do
  begin
    ProvinceSet.Add(StrToInt(ProvinceNoList[i]));
  end;

  for i := 0 to StateDataList.Count - 1 do
  begin
    TmpStr := StateDataList[i];
    if (TmpStr.Contains('history')) then
    begin
      InsertIndex := 0;
      TmpStr := Copy(TmpStr, TmpStr.IndexOf('history') + 1);
    end;
    if ((InsertIndex >= 0) AND (TmpStr.Contains('{'))) then
    begin
      Inc(InsertIndex);
    end;
    if ((InsertIndex >= 0) AND (TmpStr.Contains('}'))) then
    begin
      Dec(InsertIndex);
      if (InsertIndex = 0) then
      begin
        InsertIndex := i;
        Break;
      end;
    end;
    if ((InsertIndex = 1) AND TmpStr.Contains('owner')) then
    begin
      CountryTag := TmpStr.Trim();
      CountryTag := Copy(CountryTag, Length(CountryTag) - 2, 3); // GET last 3 chars
    end;
  end;
  FreeAndNil(ProvinceNoList);
  FreeAndNil(StateDataList);
end;

procedure TForm1.AddNeighbor(var NeighborMap: TNeighborMap; Key: Integer;
  Value: IntArray);
var
  i, len, index: Integer;
  Arr: IntArray;
begin
  index := NeighborMap.IndexOf(Key);
  if (index >= 0) then
    Arr := NeighborMap.Data[index]
  else
    Arr := [];

  for i := 0 to Length(Value) - 1 do
  begin
    if (ArrayHasInt(Arr, Value[i])) then
      Continue;
    len := Length(Arr);
    SetLength(Arr, len + 1);
    Arr[len] := Value[i];
  end;

  if (index >= 0) then
    NeighborMap.Data[index] := Arr
  else
    NeighborMap.Add(Key, Arr);
end;

procedure TForm1.RemoveNeighbor(var NeighborMap: TNeighborMap; Key: Integer;
  Value: IntArray);
var
  i, len, index: Integer;
  Arr, Res: IntArray;
begin
  index := NeighborMap.IndexOf(Key);
  if (index < 0) then
    Exit;

  Arr := NeighborMap.Data[index];
  SetLength(Res, 0);
  for i := 0 to Length(Arr) - 1 do
  begin
    if (ArrayHasInt(Value, Arr[i])) then
      Continue;
    len := Length(Res);
    SetLength(Res, len + 1);
    Res[len] := Arr[i];
  end;
  NeighborMap.Data[index] := Res;
end;

function TForm1.ArrayHasInt(Arr: IntArray; x: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(Arr) - 1 do
    if (Arr[i] = x) then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

function TForm1.IntArrayJoin(Value: IntArray): string;
var
  i: Integer;
begin
  Result := '';
  if (Assigned(Value)) then
    for i := 0 to Length(Value) - 1 do
      Result := Result + IntToStr(Value[i]) + ' ';
  Result := Result.Trim();
end;

function TForm1.IsNaval(provinceList: TStringList; provinceNo: Integer): Boolean;
var
  i: integer;
  tmp: TStringList;
begin
  tmp := TStringList.Create();
  tmp.Delimiter := ';';

  for i := 0 to provinceList.Count - 1 do
  begin
    tmp.Clear();
    if (provinceList[i] = '') then
      Continue;
    tmp.DelimitedText := provinceList[i];
    if (tmp[0] = IntToStr(provinceNo)) then
    begin
      Result := (tmp[5] = 'true');
      FreeAndNil(tmp);
      Exit;
    end;
  end;
  FreeAndNil(tmp);
  raise EMathError.Create('Province ' + IntToStr(provinceNo) + 'not found');
end;

end.

