{
  Copyright 2023-2023 Michalis Kamburelis, Freedomax.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit CastleInternalAnimationPlayerDialog;

{$I castleconf.inc}

interface

uses
  Generics.Collections, Contnrs, Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, ButtonPanel, StdCtrls, ExtCtrls, Menus, ComCtrls,
  CastleAnimationPlayer, CastleControl, CastleControls, CastleUIControls,
  CastleVectors, CastleColors, CastleKeysMouse, RttiUtils, CastleTransform,
  CastleViewport;

type
  TAnimationPlayerView = class(TCastleView)
  private
    FAnimationPlayer: TAnimationPlayer;
    FRoot: TCastleUserInterface;
    FTrackRoot: TCastleVerticalGroup;
    procedure SetAnimationPlayer(const AValue: TAnimationPlayer);
  protected
    AddKeyFrameButton: TCastleButton;
    procedure ReloadTracks;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;

    procedure AddTrack(const ATrack: TAnimationTrack);

    property AnimationPlayer: TAnimationPlayer
      read FAnimationPlayer write SetAnimationPlayer;
  end;

  TAnimationPlayerDialog = class(TForm)
    ButtonNewAnimation: TButton;
    ButtonDeleteAnimation: TButton;
    ButtonNewTrack: TButton;
    ButtonDeleteTrack: TButton;
    ButtonPanel1: TButtonPanel;
    CastleControl1: TCastleControl;
    ComboBoxAnimation: TComboBox;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    PopupMenuAddTrack: TPopupMenu;
    procedure ButtonNewAnimationClick(Sender: TObject);
    procedure ButtonNewTrackClick(Sender: TObject);
    procedure ComboBoxAnimationChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure MenuItem1Click(Sender: TObject);
  private
    FView: TAnimationPlayerView;
    FDisableAnimationSelect: boolean;

    function GetAnimationPlayer: TAnimationPlayer;

    procedure InitView;
    procedure AnimationListChanged;
    procedure UpdateUIControls;
    procedure EnableAnimationSelect(AEnable: boolean);

    property AnimationPlayer: TAnimationPlayer read GetAnimationPlayer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(const AAnimationPlayer: TAnimationPlayer);
  end;

implementation

uses Math,
  CastleLclUtils, castleinternalpropertyselectdialog;

{$R *.lfm}

procedure TAnimationPlayerView.SetAnimationPlayer(const AValue: TAnimationPlayer);
begin
  if FAnimationPlayer = AValue then Exit;

  FAnimationPlayer := AValue;

end;

procedure TAnimationPlayerView.ReloadTracks;
var
  Track: TAnimationTrack;
  R: TCastleRectangleControl;
  TrackLabel: TCastleLabel;
begin
  FTrackRoot.ClearControls;
  AddKeyFrameButton.Exists := False;
  if not Assigned(FAnimationPlayer) then Exit;
  if not Assigned(FAnimationPlayer.CurrentAnimation) then
    Exit;

  AddKeyFrameButton.Exists := FAnimationPlayer.CurrentAnimation.TrackList.Count > 0;
  for Track in FAnimationPlayer.CurrentAnimation.TrackList do
  begin
    R := TCastleRectangleControl.Create(self);
    R.Color := CastleColors.Black;
    R.Height := 24;
    FTrackRoot.InsertFront(R);

    TrackLabel := TCastleLabel.Create(self);
    TrackLabel.Caption := Track.ClassName;
    TrackLabel.Anchor(vpTop, vpTop);
    TrackLabel.Anchor(hpLeft, hpLeft);
    TrackLabel.Color := CastleColors.White;
    R.InsertFront(TrackLabel);
  end;
end;

{ TAnimationPlayerView ---------------------------------------------------- }
procedure TAnimationPlayerView.Start;
begin
  inherited Start;
  FRoot := TCastleUserInterface.Create(self);
  FRoot.FullSize := True;
  self.InsertFront(FRoot);
  FTrackRoot := TCastleVerticalGroup.Create(self);
  FTrackRoot.FullSize := True;
  FRoot.InsertFront(FTrackRoot);
  AddKeyFrameButton := TCastleButton.Create(self);
  AddKeyFrameButton.Caption := '+';
  AddKeyFrameButton.Anchor(vpTop, vpTop);
  AddKeyFrameButton.Anchor(hpLeft, hpLeft);
  AddKeyFrameButton.AutoSize := False;
  AddKeyFrameButton.Height := 20;
  AddKeyFrameButton.Width := 20;
  FRoot.InsertFront(AddKeyFrameButton);
end;

procedure TAnimationPlayerView.Update(const SecondsPassed: single;
  var HandleInput: boolean);
begin
  inherited Update(SecondsPassed, HandleInput);

end;

function TAnimationPlayerView.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited Press(Event);
end;

function TAnimationPlayerView.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited Motion(Event);
  AddKeyFrameButton.Translation :=
    Vector2(Event.Position.X - AddKeyFrameButton.Width / 2, 0);
end;

procedure TAnimationPlayerView.AddTrack(const ATrack: TAnimationTrack);
begin
  if not Assigned(FAnimationPlayer.CurrentAnimation) then
  begin
    ShowMessage('Please select an animation first.');
    Exit;
  end;
  FAnimationPlayer.CurrentAnimation.AddTrack(ATrack);
  ReloadTracks;
end;

{ TAnimationPlayerDialog ---------------------------------------------------- }

constructor TAnimationPlayerDialog.Create(AOwner: TComponent);
begin
  inherited;
  InitView;
end;

destructor TAnimationPlayerDialog.Destroy;
begin
  inherited;
end;

procedure TAnimationPlayerDialog.Load(const AAnimationPlayer: TAnimationPlayer);
begin
  FView.AnimationPlayer := AAnimationPlayer;
  AnimationListChanged;
end;

procedure TAnimationPlayerDialog.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
  begin

  end;
end;

procedure TAnimationPlayerDialog.MenuItem1Click(Sender: TObject);
var
  Form: TPropertySelectForm;
  comp: TComponent;
  ARoot: TCastleUserInterface;
  Track: TAnimationPropertyTrack;
begin
  Form := TPropertySelectForm.Create(nil);
  try
    comp := FView.AnimationPlayer.Owner;
    if comp is TCastleUserInterface then
      ARoot := (comp as TCastleUserInterface)
    else
    if comp is TCastleTransform then
      ARoot := ((comp as TCastleTransform).World.Owner as TCastleUserInterface)
    else
      raise Exception.Create('Cannot find root TCastleUserInterface');

    while Assigned(ARoot.Parent) do ARoot := ARoot.Parent;

    Form.Load(ARoot);

    if Form.ShowModal = mrOk then
    begin
      if Assigned(Form.SelectedObject) and (Form.SelectedProperty <> '') then
      begin
        Track := TAnimationPropertyTrack.Create(Form.SelectedObject,
          Form.SelectedProperty);
        FView.AddTrack(Track);
      end
      else
        ShowMessage('Did not complete the selection.');
    end;
  finally
    FreeAndNil(Form);
  end;
end;

function TAnimationPlayerDialog.GetAnimationPlayer: TAnimationPlayer;
begin
  Result := FView.AnimationPlayer;
end;

procedure TAnimationPlayerDialog.ButtonNewTrackClick(Sender: TObject);
var
  pt: TPoint;
begin
  PopupMenuAddTrack.PopupComponent := ButtonNewTrack;
  pt := ButtonNewTrack.ClientToScreen(Point(0, ButtonNewTrack.Height));
  PopupMenuAddTrack.Popup(pt.x, pt.y);
end;

procedure TAnimationPlayerDialog.ComboBoxAnimationChange(Sender: TObject);
begin
  if not Assigned(AnimationPlayer) then Exit;
  if FDisableAnimationSelect then Exit;

  AnimationPlayer.Animation := ComboBoxAnimation.Text;
  UpdateUIControls;
  FView.ReloadTracks;
  ShowMessage('changed to ' + ComboBoxAnimation.Text);
end;

procedure TAnimationPlayerDialog.ButtonNewAnimationClick(Sender: TObject);
var
  AName: string;
begin
  if InputQuery('NewAnimation', 'Input name:', AName) then
  begin
    AnimationPlayer.NewAnimation(AName);
    AnimationListChanged;
  end;
end;

procedure TAnimationPlayerDialog.InitView;
begin
  if not Assigned(FView) then
  begin
    FView := TAnimationPlayerView.Create(CastleControl1);
    CastleControl1.Container.View := FView;
    CastleControl1.Container.BackgroundColor := CastleColors.Gray;
  end;
end;

procedure TAnimationPlayerDialog.AnimationListChanged;
var
  AName, OldSelectedName: string;
  AIndex: integer;
begin
  AIndex := 0;
  EnableAnimationSelect(False);
  OldSelectedName := ComboBoxAnimation.Text;
  ComboBoxAnimation.Items.BeginUpdate;
  try
    ComboBoxAnimation.Clear;
    ComboBoxAnimation.Items.Add('');
    for AName in AnimationPlayer.AnimationList.Keys do
    begin
      ComboBoxAnimation.Items.Add(AName);
      if AName = OldSelectedName then AIndex := ComboBoxAnimation.Items.Count - 1;
    end;
    ComboBoxAnimation.ItemIndex := AIndex;
  finally
    EnableAnimationSelect(True);
    ComboBoxAnimation.Items.EndUpdate;
    UpdateUIControls;
    FView.ReloadTracks;
  end;

end;

procedure TAnimationPlayerDialog.UpdateUIControls;
begin
  ButtonNewAnimation.Enabled :=
    Assigned(AnimationPlayer);
  ButtonDeleteAnimation.Enabled :=
    Assigned(AnimationPlayer) and Assigned(AnimationPlayer.CurrentAnimation);
  ButtonNewTrack.Enabled := Assigned(AnimationPlayer) and
    Assigned(AnimationPlayer.CurrentAnimation);
  ButtonDeleteTrack.Enabled :=
    Assigned(AnimationPlayer) and Assigned(AnimationPlayer.CurrentAnimation);
end;

procedure TAnimationPlayerDialog.EnableAnimationSelect(AEnable: boolean);
begin
  FDisableAnimationSelect := not AEnable;
end;

end.
