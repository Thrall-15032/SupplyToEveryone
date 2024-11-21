unit Unit3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image0: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label0: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form2: TForm2;

implementation

uses
  Utils;

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  Image0.ImageIndex := 2 - IsCountryTagsGame.ToInteger;
  Image1.ImageIndex := 2 - IsStatesGame.ToInteger;
  Image2.ImageIndex := 2 - IsDefinitionGame.ToInteger;
  Image3.ImageIndex := 2 - IsProvinceMapGame.ToInteger;
  Image4.ImageIndex := 2 - IsAdjacencyGame.ToInteger;
  Image5.ImageIndex := 2 - IsDescriptorMod.ToInteger;
end;

end.

