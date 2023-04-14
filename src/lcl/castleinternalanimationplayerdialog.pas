{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Dialog for selecting Tiled layers to show, used by CastlePropEdits. }
unit CastleInternalAnimationPlayerDialog;

{$I castleconf.inc}

interface

uses
  Generics.Collections, Contnrs,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls, CastleAnimationPlayer;

type
  TAnimationPlayerDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckLayers: TScrollBox;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure SelectAllButtonClick(Sender: TObject);
  private
    Checkboxes: TObjectList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(const AAnimationPlayer: TAnimationPlayer);
    procedure UpdateSelectionUi;
  end;

implementation

uses Math,
  CastleLclUtils;

{$R *.lfm}

{ TAnimationPlayerDialog ---------------------------------------------------- }

constructor TAnimationPlayerDialog.Create(AOwner: TComponent);
begin
  inherited;
  Checkboxes := TObjectList.Create(False);
end;

destructor TAnimationPlayerDialog.Destroy;
begin
  FreeAndNil(Checkboxes);
  inherited;
end;

procedure TAnimationPlayerDialog.Load(const AAnimationPlayer: TAnimationPlayer);
begin

end;

procedure TAnimationPlayerDialog.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  I: integer;
begin
  if ModalResult = mrOk then
  begin

  end;
end;

procedure TAnimationPlayerDialog.SelectAllButtonClick(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to Checkboxes.Count - 1 do
    TCheckBox(Checkboxes[I]).Checked := True;
end;

procedure TAnimationPlayerDialog.UpdateSelectionUi;
var
  S: string;
  Checkbox: TCheckBox;
  I: integer;
begin
  Checkboxes.Clear;

end;

end.
