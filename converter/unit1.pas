unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DialogRes, LCLType, ComCtrls, fgl;

type
  IntArray = array of Integer;

  TProvinceSet = specialize TFPGList<Integer>;
  TProvinceMap = specialize TFPGMap<TColor, Integer>;
  TNeighborMap = specialize TFPGMap<Integer, IntArray>;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    function ScanProvinces(): TProvinceSet;
    procedure AddNeighbor(var NeighborMap: TNeighborMap; Key: Integer; Value: IntArray);
    procedure RemoveNeighbor(var NeighborMap: TNeighborMap; Key: Integer; Value: IntArray);
    function ArrayHasInt(Arr: IntArray; x: Integer): Boolean;
    function IntArrayJoin(Value: IntArray): string;
    function IsNaval(provinceList: TStringList; provinceNo: string): Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

uses
  FileUtil;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  StatesPath, FileName, TmpStr, ResStr: string;
  ProvinceDataList, StatesList, StateDataList, ProvinceNoList: TStringList;
  i, j, index, a: Integer;
begin
  Button1.Enabled := false;
  StatesPath := Edit1.Text + 'history\states\';
  ProvinceDataList := TStringList.Create();
  ProvinceDataList.LoadFromFile(Edit1.Text + 'map\definition.csv');
  StatesList := TStringList.Create();
  FileUtil.FindAllFiles(StatesList, StatesPath, '*.txt', False, 0);

  ProgressBar1.Min := 0;
  ProgressBar1.Max := statesList.Count - 1;

  for i := 0 to statesList.Count - 1 do
  begin
    // find province={...} param and parse provinceIds
    StateDataList := TStringList.Create();
    StateDataList.LoadFromFile(StatesList[i]);
    index := StateDataList.Text.IndexOf('provinces');
    TmpStr := Copy(StateDataList.Text, index, Length(StateDataList.Text));
    index := TmpStr.IndexOf('}');
    TmpStr := Copy(TmpStr, 1, index + 1);
    TmpStr := StringReplace(TmpStr, 'provinces', '', [rfReplaceAll]);
    TmpStr := StringReplace(TmpStr, '=', '', [rfReplaceAll]);
    TmpStr := StringReplace(TmpStr, '{', '', [rfReplaceAll]);
    TmpStr := StringReplace(TmpStr, '}', '', [rfReplaceAll]);
    ProvinceNoList := TStringList.Create();
    ProvinceNoList.DelimitedText := TmpStr;
    ProvinceNoList.Sort();
    FileName := StringReplace(StatesList[i], StatesPath, '', [rfIgnoreCase]);

    // find where end history={...} param for insert new building
    a := -1;
    for j := 0 to StateDataList.Count - 1 do
    begin
      if (StateDataList[j].Contains('history')) then
      begin
        a := 0;
      end;
      if ((a >= 0) AND (StateDataList[j].Contains('{'))) then
      begin
        Inc(a);
      end;
      if ((a >= 0) AND (StateDataList[j].Contains('}'))) then
      begin
        Dec(a);
        if (a = 0) then
        begin
          a := j;
          Break;
        end;
      end;
    end;

    // insert new buildings={...} param
    StateDataList.Insert(a, #9#9 + 'buildings={');
    Inc(a);
    StateDataList.Insert(a, #9#9#9 + 'infrastructure=5');
    Inc(a);
    for j := 0 to ProvinceNoList.Count - 1 do
    begin
      StateDataList.Insert(a, #9#9#9 + ProvinceNoList[j] + '={');
      Inc(a);
      StateDataList.Insert(a, #9#9#9#9 + 'supply_node=1');
      Inc(a);
      if (IsNaval(ProvinceDataList, ProvinceNoList[j])) then
      begin
        StateDataList.Insert(a, #9#9#9#9 + 'naval_base=10');
        Inc(a);
      end;
      StateDataList.Insert(a, #9#9#9 + '}');
      Inc(a);
    end;
    StateDataList.Insert(a, #9#9 + '}');
    Inc(a);

    StateDataList.SaveToFile('.\tmp\' + FileName);

    FreeAndNil(ProvinceNoList);
    FreeAndNil(StateDataList);

    ProgressBar1.Position := i;
    Application.ProcessMessages();
  end;

  FreeAndNil(StatesList);
  FreeAndNil(ProvinceDataList);

  ShowMessage('done');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  TmpList, ResList, ProvinceDataList, AdjacencyList, TrainwayList, ErrorList: TStringList;
  TmpStr: string;
  ProvinceSet: TProvinceSet;
  ProvinceMap: TProvinceMap;
  WorldBmp: TBitmap;
  NeighborMap: TNeighborMap;
  i, j, index, indexN, CurProvince: Integer;
  ArrN: IntArray;
begin
  Button2.Enabled := false;
  // File definition.csv to Map<ProvinceColor, ProvinceId>
  ProvinceDataList := TStringList.Create();
  ProvinceDataList.LoadFromFile(Edit1.Text + 'map\definition.csv');
  ProvinceMap := TProvinceMap.Create();
  NeighborMap := TNeighborMap.Create();
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
  ProgressBar1.Min := 0;
  ProgressBar1.Max := WorldBmp.Width - 1;
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
    ProgressBar1.Position := i;
    Application.ProcessMessages();
  end;

  // Apply data from file adjacencies.csv to Map<ProvinceId, NeighborIds[]>
  ProvinceSet := ScanProvinces();
  AdjacencyList := TStringList.Create();
  AdjacencyList.LoadFromFile(Edit1.Text + 'map\adjacencies.csv');
  ErrorList := TStringList.Create();
  ProgressBar1.Min := 0;
  ProgressBar1.Max := AdjacencyList.Count - 1;
  for i := 0 to AdjacencyList.Count - 1 do
  begin
    TmpList := TStringList.Create();
    TmpList.Delimiter := ';';
    TmpList.DelimitedText := AdjacencyList[i];
    if ((TmpList[0] = 'From') OR (TmpList[0] = '-1')) then
    begin
      FreeAndNil(TmpList);
      Continue;
    end;
    if ((ProvinceSet.IndexOf(StrToInt(TmpList[0])) < 0)
      OR (ProvinceSet.IndexOf(StrToInt(TmpList[1])) < 0)) then
    begin
      ErrorList.Add(AdjacencyList[i]);
      FreeAndNil(TmpList);
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
    FreeAndNil(TmpList);
    ProgressBar1.Position := i;
    Application.ProcessMessages();
  end;
  ErrorList.SaveToFile('.\error_adjacencies.txt');

  // Map<ProvinceId, NeighborIds[]> to string "ProvinceId Neigh1 Neigh2 NeighN"
  // Map<ProvinceId, NeighborIds[]> to train data "5 2 ProvinceId1 ProvinceId12"
  NeighborMap.Sort();
  ResList := TStringList.Create();
  TrainwayList := TStringList.Create();
  ProgressBar1.Min := 0;
  ProgressBar1.Max := NeighborMap.Count - 1;
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
        TrainwayList.Add('5 2 ' + IntToStr(NeighborMap.Keys[i])
          + ' ' + IntToStr(NeighborMap.Data[i][j]));
      end;
    end;
    ProgressBar1.Position := i;
    Application.ProcessMessages();
  end;

  ResList.SaveToFile('.\neighbor.txt');
  TrainwayList.SaveToFile('.\railways.txt');
  FreeAndNil(TrainwayList);
  FreeAndNil(ResList);
  FreeAndNil(WorldBmp);
  FreeAndNil(NeighborMap);
  FreeAndNil(ProvinceMap);
  FreeAndNil(ProvinceDataList);

  ShowMessage('done');
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ProvinceSet: TProvinceSet;
begin
  ProvinceSet := ScanProvinces();
  ShowMessage(IntToStr(ProvinceSet.Count));

  FreeAndNil(ProvinceSet);
end;

function TForm1.ScanProvinces(): TProvinceSet;
var
  StatesPath, TmpStr: string;
  StatesList, StateDataList, ProvinceNoList: TStringList;
  i, j, index: Integer;
begin
  Result := TProvinceSet.Create();
  StatesPath := Edit1.Text + 'history\states\';
  StatesList := TStringList.Create();
  FileUtil.FindAllFiles(StatesList, StatesPath, '*.txt', False, 0);

  ProgressBar1.Min := 0;
  ProgressBar1.Max := statesList.Count - 1;

  for i := 0 to statesList.Count - 1 do
  begin
    StateDataList := TStringList.Create();
    StateDataList.LoadFromFile(StatesList[i]);
    index := StateDataList.Text.IndexOf('provinces');
    TmpStr := Copy(StateDataList.Text, index, Length(StateDataList.Text));
    index := TmpStr.IndexOf('}');
    TmpStr := Copy(TmpStr, 1, index + 1);
    TmpStr := StringReplace(TmpStr, 'provinces', '', [rfReplaceAll]);
    TmpStr := StringReplace(TmpStr, '=', '', [rfReplaceAll]);
    TmpStr := StringReplace(TmpStr, '{', '', [rfReplaceAll]);
    TmpStr := StringReplace(TmpStr, '}', '', [rfReplaceAll]);
    ProvinceNoList := TStringList.Create();
    ProvinceNoList.DelimitedText := TmpStr;
    for j := 0 to ProvinceNoList.Count - 1 do
    begin
      Result.Add(StrToInt(ProvinceNoList[j]));
    end;
    FreeAndNil(ProvinceNoList);
    FreeAndNil(StateDataList);
    ProgressBar1.Position := i;
    Application.ProcessMessages();
  end;

  FreeAndNil(StatesList);
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

function TForm1.IsNaval(provinceList: TStringList; provinceNo: string): Boolean;
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
    if (tmp[0] = provinceNo) then
    begin
      Result := (tmp[5] = 'true');
      FreeAndNil(tmp);
      Exit;
    end;
  end;
  FreeAndNil(tmp);
  raise EMathError.Create('Province ' + provinceNo + 'not found');
end;

end.

