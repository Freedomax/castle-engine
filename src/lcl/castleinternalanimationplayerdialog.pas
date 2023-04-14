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
  CastleViewport, CastleTimeUtils, Variants, CastleRectangles;

type
  TCastleTrackView = class(TCastleRectangleControl)
  private
  const
    PixelsEachSceond: single = 200;
  var
    FTrack: TAnimationTrack;
    procedure SetTrack(const AValue: TAnimationTrack);
    function TimePosition(const ATime: TFloatTime): single;
  public

    procedure Render; override;
    property Track: TAnimationTrack read FTrack write SetTrack;
  end;

  TAnimationPlayerView = class(TCastleView)
  private
    FAnimationPlayer: TAnimationPlayer;
    FRoot: TCastleUserInterface;
    FTrackListView: TCastleVerticalGroup;
    procedure AddKeyFrameButtonClick(Sender: TObject);
    procedure SetAnimationPlayer(const AValue: TAnimationPlayer);
  protected
    ButtonAddKeyFrame: TCastleButton;
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
    CheckBoxContinuous: TCheckBox;
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

    function GetAnimationPlayer: TAnimationPlayer;

    procedure InitView;
    procedure AnimationListChanged;
    procedure UpdateUIControls;

    property AnimationPlayer: TAnimationPlayer read GetAnimationPlayer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(const AAnimationPlayer: TAnimationPlayer);
  end;

implementation

uses Math,
  CastleLclUtils, castleinternalpropertyselectdialog, CastleGLUtils, CastleRenderOptions;

{$R *.lfm}

procedure TCastleTrackView.SetTrack(const AValue: TAnimationTrack);
begin
  if FTrack <> AValue then
  begin
    FTrack := AValue;
  end;
end;

function TCastleTrackView.TimePosition(const ATime: TFloatTime): single;
begin
  Result := RenderRect.Left + ATime * PixelsEachSceond * UIScale;
end;

procedure TCastleTrackView.Render;
var
  KeyFrame: TAnimationTrack.TAnimationKeyframe;
  FramePos: single;
  R: TFloatRectangle;

  procedure RenderLine(const X: single; const LineColor: TCastleColor;
  const LineWidth: single);
  var
    arr: array[0..1] of TVector2;
  begin
    arr[0] := Vector2(X, R.Bottom + 2 * UIScale);
    arr[1] := Vector2(X, R.Top - 2 * UIScale);
    DrawPrimitive2D(pmLines,
      arr,
      LineColor, bsSrcAlpha, bdOneMinusSrcAlpha, False, LineWidth);
  end;

  procedure RenderDetail;
  var
    v: variant;
  begin
    v := KeyFrame.Value;
    if VarIsArray(v) then
    begin
    end
    else
    begin
      UIFont.Print(FramePos, R.Bottom + 2 * UIScale, CastleColors.White, v);
    end;

  end;

  procedure RenderKeyFrame;
  begin
    RenderLine(FramePos, CastleColors.White, 1 * UIScale);
    RenderDetail;
  end;

begin
  inherited Render;
  if not Assigned(FTrack) then Exit;

  R := RenderRect;
  for KeyFrame in FTrack.KeyframeList do
  begin
    FramePos := TimePosition(KeyFrame.Time);
    RenderKeyFrame;
  end;
end;

procedure TAnimationPlayerView.SetAnimationPlayer(const AValue: TAnimationPlayer);
begin
  if FAnimationPlayer = AValue then Exit;

  FAnimationPlayer := AValue;

end;

procedure TAnimationPlayerView.AddKeyFrameButtonClick(Sender: TObject);
begin
  if not Assigned(AnimationPlayer) then Exit;
  if not Assigned(AnimationPlayer.CurrentAnimation) then Exit;
  if AnimationPlayer.CurrentAnimation.TrackList.Count = 0 then Exit;

  AnimationPlayer.CurrentAnimation.TrackList.Items[Random(
    AnimationPlayer.CurrentAnimation.TrackList.Count)].AddKeyframe(
    Random(100) / 50, Random(2));
end;

procedure TAnimationPlayerView.ReloadTracks;
var
  ATrack: TAnimationTrack;
  ATrackView: TCastleTrackView;
begin
  FTrackListView.ClearControls;
  ButtonAddKeyFrame.Exists := False;
  if not Assigned(FAnimationPlayer) then Exit;
  if not Assigned(FAnimationPlayer.CurrentAnimation) then
    Exit;

  ButtonAddKeyFrame.Exists := FAnimationPlayer.CurrentAnimation.TrackList.Count > 0;
  for ATrack in FAnimationPlayer.CurrentAnimation.TrackList do
  begin
    ATrackView := TCastleTrackView.Create(self);
    ATrackView.Color := Vector4(Random(256) / 255, Random(256) / 255,
      Random(256) / 255, 1);
    ATrackView.Track := ATrack;
    ATrackView.Height := 24;
    FTrackListView.InsertFront(ATrackView);
  end;
end;

{ TAnimationPlayerView ---------------------------------------------------- }
procedure TAnimationPlayerView.Start;
var
  AScrollView: TCastleScrollView;
begin
  inherited Start;
  FRoot := TCastleUserInterface.Create(self);
  FRoot.FullSize := True;
  self.InsertFront(FRoot);

  AScrollView := TCastleScrollView.Create(Self);
  FRoot.InsertFront(AScrollView);
  AScrollView.FullSize := True;
  AScrollView.EnableDragging := True;

  FTrackListView := TCastleVerticalGroup.Create(self);
  FTrackListView.FullSize := False;
  AScrollView.ScrollArea.InsertFront(FTrackListView);

  ButtonAddKeyFrame := TCastleButton.Create(self);
  ButtonAddKeyFrame.OnClick :={$IFDEF FPC}@{$ENDIF}AddKeyFrameButtonClick;
  ButtonAddKeyFrame.Caption := '+';
  ButtonAddKeyFrame.Anchor(vpTop, vpTop);
  ButtonAddKeyFrame.Anchor(hpLeft, hpLeft);
  ButtonAddKeyFrame.AutoSize := False;
  ButtonAddKeyFrame.Height := 20;
  ButtonAddKeyFrame.Width := 20;
  FRoot.InsertFront(ButtonAddKeyFrame);
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
  ButtonAddKeyFrame.Translation :=
    Vector2(Event.Position.X - ButtonAddKeyFrame.Width / 2, 0);
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
        if CheckBoxContinuous.Checked then
          Track.Mode := tmContinuous
        else
          Track.Mode := tmDiscrete;
        FView.AddTrack(Track);
      end
      else
        ShowMessage('Didnot complete the selection.');
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

  AnimationPlayer.Animation := ComboBoxAnimation.Text;
  UpdateUIControls;
  FView.ReloadTracks;
end;

procedure TAnimationPlayerDialog.ButtonNewAnimationClick(Sender: TObject);
var
  AName: string;
begin
  if InputQuery('NewAnimation', 'Input name:', AName) then
  begin
    AnimationPlayer.NewAnimation(AName);
    AnimationPlayer.Animation := AName;
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
  AName: string;
begin
  ComboBoxAnimation.Items.BeginUpdate;
  try
    ComboBoxAnimation.Clear;
    ComboBoxAnimation.Items.Add('');
    for AName in AnimationPlayer.AnimationList.Keys do
      ComboBoxAnimation.Items.Add(AName);

    ComboBoxAnimation.ItemIndex :=
      ComboBoxAnimation.Items.IndexOf(AnimationPlayer.Animation);
  finally
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

end.
