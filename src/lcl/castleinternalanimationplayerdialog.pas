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
  Dialogs, ButtonPanel, StdCtrls, ExtCtrls, Menus, ComCtrls, Buttons,
  CastleAnimationPlayer, CastleControl, CastleControls, CastleUIControls,
  CastleVectors, CastleColors, CastleKeysMouse, RttiUtils, CastleTransform,
  CastleViewport, CastleTimeUtils, Variants, CastleRectangles;

type
  TTrackView = class(TCastleRectangleControl)
  private
  var
    FTrack: TAnimationTrack;
    procedure SetTrack(const AValue: TAnimationTrack);
  public
  const
    PixelsEachSceond: single = 200;
    procedure Render; override;
    property Track: TAnimationTrack read FTrack write SetTrack;
  end;

  TAnimationPlayerView = class(TCastleView)
  private
    FPlaying: boolean;
    FPlayingChanged: TNotifyEvent;
  type
    TTrackViewList = {$Ifdef fpc}specialize{$endif}TObjectList<TTrackView>;
  const
    TrackHeight = 100;
    ItemFontSize = 15;
    ItemFontSmallSize = 12;
    ItemSpacing = 2;
  var
    FAnimationPlayer: TAnimationPlayer;
    FRoot: TCastleUserInterface;
    FTrackListView: TCastleVerticalGroup;
    FTrackViewList: TTrackViewList;
    procedure AButtonDeleteClick(Sender: TObject);
    procedure ACheckBoxChange(Sender: TObject);
    procedure AddKeyFrameButtonClick(Sender: TObject);
    procedure FAnimationPlayerAnimationComplete(Sender: TObject);
    { Track index, "-1" means all changed. }
    procedure KeyFrameListChanged(const Index: integer);
    function GetCurrentAnimation: TAnimation;
    procedure SetAnimationPlayer(const AValue: TAnimationPlayer);
    procedure SetPlaying(const AValue: boolean);
    procedure SetPlayingChanged(const AValue: TNotifyEvent);

  protected
    ButtonAddKeyFrame: TCastleButton;
    procedure ReloadTracks;
  public
    procedure Start; override;
    procedure Stop; override;
    procedure RenderOverChildren; override;
    procedure Update(const SecondsPassed: single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;

    procedure AddTrack(const ATrack: TAnimationTrack);

    property AnimationPlayer: TAnimationPlayer
      read FAnimationPlayer write SetAnimationPlayer;
    property CurrentAnimation: TAnimation read GetCurrentAnimation;
    property Playing: boolean read FPlaying write SetPlaying;
    property PlayingChanged: TNotifyEvent read FPlayingChanged write SetPlayingChanged;
  end;

  TAnimationPlayerDialog = class(TForm)
    ButtonNewAnimation: TButton;
    ButtonPlayStop: TSpeedButton;
    ButtonRemoveAnimation: TButton;
    ButtonNewTrack: TButton;
    CastleControl1: TCastleControl;
    ComboBoxAnimation: TComboBox;
    ComboBoxPlayStyle: TComboBox;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    PopupMenuAddTrack: TPopupMenu;
    procedure ButtonRemoveAnimationClick(Sender: TObject);
    procedure ButtonNewAnimationClick(Sender: TObject);
    procedure ButtonNewTrackClick(Sender: TObject);
    procedure ButtonPlayStopClick(Sender: TObject);
    procedure ComboBoxAnimationChange(Sender: TObject);
    procedure ComboBoxPlayStyleChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure MenuItem1Click(Sender: TObject);
  private
    FView: TAnimationPlayerView;

    procedure FViewPlayingChanged(Sender: TObject);
    function GetAnimationPlayer: TAnimationPlayer;

    procedure InitView;
    procedure InitControls;
    procedure AnimationListChanged;
    procedure CurrentAnimationChanged;
    procedure UpdateUIControls;
    function GetCurrentAnimation: TAnimation;

    property AnimationPlayer: TAnimationPlayer read GetAnimationPlayer;
    property CurrentAnimation: TAnimation read GetCurrentAnimation;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(const AAnimationPlayer: TAnimationPlayer);
  end;

implementation

uses Math,
  CastleLclUtils, castleinternalpropertyselectdialog, CastleGLUtils,
  CastleRenderOptions, TypInfo, CastleStringUtils, CastleUtils;

{$R *.lfm}

procedure TTrackView.SetTrack(const AValue: TAnimationTrack);
begin
  if FTrack <> AValue then
  begin
    FTrack := AValue;
  end;
end;

procedure TTrackView.Render;
var
  KeyFrame: TAnimationTrack.TAnimationKeyframe;
  FramePos: single;
  R: TFloatRectangle;

  function TimeRenderPosition(const ATime: TFloatTime): single;
  begin
    Result := R.Left + ATime * PixelsEachSceond * UIScale;
  end;

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

  procedure RenderKeyFrame;
  begin
    RenderLine(FramePos, CastleColors.White, 1 * UIScale);
    //RenderDetail;
  end;

begin
  inherited Render;
  if not Assigned(FTrack) then Exit;

  R := RenderRect;
  for KeyFrame in FTrack.KeyframeList do
  begin
    FramePos := TimeRenderPosition(KeyFrame.Time);
    RenderKeyFrame;
  end;
end;

procedure TAnimationPlayerView.SetAnimationPlayer(const AValue: TAnimationPlayer);
begin
  if FAnimationPlayer <> AValue then
  begin
    if Assigned(FAnimationPlayer) then FAnimationPlayer.OnAnimationComplete := nil;
    FAnimationPlayer := AValue;
    FAnimationPlayer.OnAnimationComplete :=
 {$Ifdef fpc}@{$endif}FAnimationPlayerAnimationComplete;
  end;

end;

procedure TAnimationPlayerView.SetPlaying(const AValue: boolean);
begin
  if FPlaying <> AValue then
  begin
    FPlaying := AValue;
    if Assigned(CurrentAnimation) then
    begin
      if FPlaying then CurrentAnimation.Start
      else
        CurrentAnimation.Stop;
    end;
    if Assigned(FPlayingChanged) then FPlayingChanged(Self);
  end;
end;

procedure TAnimationPlayerView.SetPlayingChanged(const AValue: TNotifyEvent);
begin
  if FPlayingChanged <> AValue then
    FPlayingChanged := AValue;
end;

procedure TAnimationPlayerView.AddKeyFrameButtonClick(Sender: TObject);
var
  Index: integer;
begin
  if not Assigned(CurrentAnimation) then Exit;
  if CurrentAnimation.TrackList.Count = 0 then Exit;

  Index := Random(CurrentAnimation.TrackList.Count);
  CurrentAnimation.TrackList.Items[Index].AddKeyframe(
    Random(100) / 50, Random(100) / 50);

  KeyFrameListChanged(Index);
end;

procedure TAnimationPlayerView.FAnimationPlayerAnimationComplete(Sender: TObject);
begin
  Playing := False;
end;

procedure TAnimationPlayerView.KeyFrameListChanged(const Index: integer);

  procedure FixSize(const AIndex: integer);
  begin
    FTrackViewList.Items[AIndex].Width :=
      TTrackView.PixelsEachSceond * CurrentAnimation.TrackList.Items[AIndex].Duration;
  end;

var
  I: integer;
begin
  if Index < 0 then
  begin
    Assert(FTrackViewList.Count <= CurrentAnimation.TrackList.Count);
    for I := 0 to FTrackViewList.Count - 1 do FixSize(I);
  end
  else
  begin
    if not Between(Index, 0, FTrackViewList.Count - 1) then
      raise Exception.Create('TAnimationPlayerView.KeyFrameListChanged out of range.');
    FixSize(Index);
  end;

end;

procedure TAnimationPlayerView.ACheckBoxChange(Sender: TObject);
var
  AIndex: integer;
begin
  AIndex := (Sender as TCastleCheckbox).Tag;
  if (Sender as TCastleCheckbox).Checked then
    CurrentAnimation.TrackList.Items[AIndex].Mode := tmContinuous
  else
    CurrentAnimation.TrackList.Items[AIndex].Mode := tmDiscrete;
end;

procedure TAnimationPlayerView.AButtonDeleteClick(Sender: TObject);
var
  AIndex: integer;
begin
  AIndex := (Sender as TCastleButton).Tag;
  CurrentAnimation.RemoveTrack(CurrentAnimation.TrackList.Items[AIndex]);
  ReloadTracks;
end;

function TAnimationPlayerView.GetCurrentAnimation: TAnimation;
begin
  if Assigned(AnimationPlayer) then
    Result := AnimationPlayer.CurrentAnimation
  else
    Result := nil;
end;

procedure TAnimationPlayerView.ReloadTracks;

  function ColorByIndex(const AIndex: integer): TCastleColor;
  begin
    { Always darker than white. Pay attention to prevent float mod}
    Result.X := (int64((AIndex + 1) * 30) mod 200) / 255;
    Result.Y := (int64((AIndex + 2) * 30) mod 200) / 255;
    Result.Z := (int64((AIndex + 3) * 30) mod 200) / 255;
    Result.W := 0.5;
  end;

var
  I: integer;
  ATrackList: TAnimationTrackList;
  ATrack: TAnimationTrack;
  ATrackContainer: TCastleHorizontalGroup;
  ATrackView: TTrackView;
  ATrackHeadView: TCastleRectangleControl;
  { HeadView items. }
  AHeadItemContainer: TCastleVerticalGroup;
  ACheckBox: TCastleCheckbox;
  ALabelPropName: TCastleLabel;
  ALabelObjectName: TCastleLabel;
  AButtonDelete: TCastleButton;
begin
  FTrackListView.ClearControls;
  ButtonAddKeyFrame.Exists := False;
  if not Assigned(CurrentAnimation) then
    Exit;

  ATrackList := CurrentAnimation.TrackList;
  ButtonAddKeyFrame.Exists := ATrackList.Count > 0;
  FTrackViewList.Clear;
  for  I := 0 to ATrackList.Count - 1 do
  begin
    ATrack := CurrentAnimation.TrackList.Items[I];
    ATrackContainer := TCastleHorizontalGroup.Create(self);
    ATrackContainer.Spacing := ItemSpacing;
    FTrackListView.InsertFront(ATrackContainer);

    ATrackHeadView := TCastleRectangleControl.Create(Self);
    ATrackHeadView.Height := TrackHeight;
    ATrackHeadView.Color := Vector4(0, 0, 0, 0.4);
    ATrackHeadView.Tag := I;
    ATrackHeadView.Width := 180;
    ATrackContainer.InsertFront(ATrackHeadView);
    { HeadView items. }
    AHeadItemContainer := TCastleVerticalGroup.Create(self);
    AHeadItemContainer.FullSize := True;
    ATrackHeadView.InsertFront(AHeadItemContainer);

    ALabelObjectName := TCastleLabel.Create(self);
    ALabelObjectName.FontSize := ItemFontSmallSize;
    ALabelObjectName.Color := CastleColors.White;
    ALabelObjectName.Caption := ATrack.FriendlyObjectName;
    AHeadItemContainer.InsertFront(ALabelObjectName);

    ALabelPropName := TCastleLabel.Create(self);
    ALabelPropName.FontSize := ItemFontSize;
    ALabelPropName.Color := CastleColors.White;
    ALabelPropName.Caption := ATrack.PropName;
    AHeadItemContainer.InsertFront(ALabelPropName);

    ACheckBox := TCastleCheckbox.Create(self);
    ACheckBox.Caption := 'Continuous';
    ACheckBox.Checked := ATrack.Mode = tmContinuous;
    ACheckBox.Tag := I;
    ACheckBox.OnChange := {$Ifdef fpc}@{$endif}ACheckBoxChange;
    ACheckBox.TextColor := CastleColors.White;
    ACheckBox.FontSize := ItemFontSize;
    AHeadItemContainer.InsertFront(ACheckBox);

    AButtonDelete := TCastleButton.Create(Self);
    AButtonDelete.Caption := 'Remove';
    AButtonDelete.OnClick := {$Ifdef fpc}@{$endif}AButtonDeleteClick;
    AButtonDelete.FontSize := ItemFontSize;
    AButtonDelete.Tag := I;
    AHeadItemContainer.InsertFront(AButtonDelete);
    { TrackView. }
    ATrackView := TTrackView.Create(self);
    ATrackView.Color := ColorByIndex(I);
    ATrackView.Track := ATrack;
    ATrackView.Height := TrackHeight;
    ATrackView.Width := 1;
    ATrackView.Tag := I;
    ATrackContainer.InsertFront(ATrackView);
    FTrackViewList.Add(ATrackView);
  end;
  KeyFrameListChanged(-1);
end;

{ TAnimationPlayerView ---------------------------------------------------- }
procedure TAnimationPlayerView.Start;
var
  AScrollView: TCastleScrollView;
begin
  inherited Start;
  FTrackViewList := TTrackViewList.Create(False);

  FRoot := TCastleUserInterface.Create(self);
  FRoot.FullSize := True;
  self.InsertFront(FRoot);

  AScrollView := TCastleScrollView.Create(Self);
  FRoot.InsertFront(AScrollView);
  AScrollView.FullSize := True;
  AScrollView.EnableDragging := True;

  FTrackListView := TCastleVerticalGroup.Create(self);
  FTrackListView.FullSize := False;
  FTrackListView.Spacing := ItemSpacing;
  FTrackListView.AutoSizeToChildren := True;
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

procedure TAnimationPlayerView.Stop;
begin
  FreeAndNil(FTrackViewList);
  inherited stop;
end;

procedure TAnimationPlayerView.RenderOverChildren;
var
  RTrack, R: TFloatRectangle;

  procedure RenderLine(const X: single; const LineColor: TCastleColor;
  const LineWidth: single);
  var
    arr: array[0..1] of TVector2;
  begin
    arr[0] := Vector2(X, R.Bottom);
    arr[1] := Vector2(X, R.Top);
    DrawPrimitive2D(pmLines,
      arr,
      LineColor, bsSrcAlpha, bdOneMinusSrcAlpha, False, LineWidth);
  end;

begin
  inherited RenderOverChildren;
  if FTrackViewList.Count = 0 then Exit;
  if not Playing then Exit;

  RTrack := FTrackViewList.First.RenderRect;
  R := RenderRect;
  RenderLine(RTrack.Left + TTrackView.PixelsEachSceond *
    CurrentAnimation.ActualCurrentTime * UIScale, CastleColors.Green, 2);
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
  if not Assigned(CurrentAnimation) then
  begin
    ShowMessage('Please select an animation first.');
    Exit;
  end;
  CurrentAnimation.AddTrack(ATrack);
  ReloadTracks;
end;

{ TAnimationPlayerDialog ---------------------------------------------------- }

constructor TAnimationPlayerDialog.Create(AOwner: TComponent);
begin
  inherited;
  InitControls;
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
      if Assigned(Form.SelectResult.SelectedObject) and
        (Form.SelectResult.SelectedProperty <> '') then
      begin
        Track := TAnimationPropertyTrack.Create(Form.SelectResult.SelectedObject,
          Form.SelectResult.SelectedProperty);
        Track.FriendlyObjectName := Form.SelectResult.FriendlyObjectName;
        FView.AddTrack(Track);
      end
      else
        ShowMessage('Didnot complete the selection.');
    end;
  finally
    FreeAndNil(Form);
  end;
end;

function TAnimationPlayerDialog.GetCurrentAnimation: TAnimation;
begin
  Result := FView.CurrentAnimation;
end;

function TAnimationPlayerDialog.GetAnimationPlayer: TAnimationPlayer;
begin
  Result := FView.AnimationPlayer;
end;

procedure TAnimationPlayerDialog.FViewPlayingChanged(Sender: TObject);
begin
  if FView.Playing then
  begin
    ButtonPlayStop.Caption := 'Stop';
  end
  else
  begin
    ButtonPlayStop.Caption := 'Start';
  end;
end;

procedure TAnimationPlayerDialog.ButtonNewTrackClick(Sender: TObject);
var
  pt: TPoint;
begin
  PopupMenuAddTrack.PopupComponent := ButtonNewTrack;
  pt := ButtonNewTrack.ClientToScreen(Point(0, ButtonNewTrack.Height));
  PopupMenuAddTrack.Popup(pt.x, pt.y);
end;

procedure TAnimationPlayerDialog.ButtonPlayStopClick(Sender: TObject);
begin
  FView.Playing := not FView.Playing;
end;

procedure TAnimationPlayerDialog.ComboBoxAnimationChange(Sender: TObject);
begin
  if not Assigned(AnimationPlayer) then Exit;

  AnimationPlayer.Animation := ComboBoxAnimation.Text;
  CurrentAnimationChanged;
end;

procedure TAnimationPlayerDialog.ComboBoxPlayStyleChange(Sender: TObject);
begin
  if Assigned(CurrentAnimation) then
    AnimationPlayer.CurrentAnimation.PlayStyle :=
      TAnimationPlayStyle(ComboBoxPlayStyle.ItemIndex);
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

procedure TAnimationPlayerDialog.ButtonRemoveAnimationClick(Sender: TObject);
begin
  if MessageDlg('Confirm', Format('Are you sure you want to delete "%s"?',
    [AnimationPlayer.Animation]), TMsgDlgType.mtConfirmation, [mbOK, mbCancel], '') =
    mrCancel then
    Exit;
  AnimationPlayer.RemoveAnimation(AnimationPlayer.Animation);
  AnimationListChanged;
end;

procedure TAnimationPlayerDialog.InitView;
begin
  if not Assigned(FView) then
  begin
    FView := TAnimationPlayerView.Create(CastleControl1);
    FView.PlayingChanged := {$Ifdef fpc}@{$endif}FViewPlayingChanged;
    CastleControl1.Container.View := FView;
    CastleControl1.Container.BackgroundColor := CastleColors.Gray;
  end;
end;

procedure TAnimationPlayerDialog.InitControls;
var
  PlayStyle: TAnimationPlayStyle;
  EnumName: string;
begin
  ComboBoxPlayStyle.Items.BeginUpdate;
  try
    ComboBoxPlayStyle.Items.Clear;
    for PlayStyle := Low(TAnimationPlayStyle) to High(TAnimationPlayStyle) do
    begin
      EnumName := GetEnumName(TypeInfo(TAnimationPlayStyle), Ord(PlayStyle));
      EnumName := PrefixRemove('aps', EnumName, True);
      ComboBoxPlayStyle.Items.Add(EnumName);
    end;

  finally
    ComboBoxPlayStyle.Items.EndUpdate;
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
  end;
  CurrentAnimationChanged;

end;

procedure TAnimationPlayerDialog.CurrentAnimationChanged;
begin
  if Assigned(CurrentAnimation) then
    ComboBoxPlayStyle.ItemIndex := Ord(AnimationPlayer.CurrentAnimation.PlayStyle);
  UpdateUIControls;
  FView.ReloadTracks;
end;

procedure TAnimationPlayerDialog.UpdateUIControls;
begin
  ButtonPlayStop.Enabled := Assigned(CurrentAnimation);
  ButtonNewAnimation.Enabled := Assigned(AnimationPlayer);
  ButtonRemoveAnimation.Enabled := Assigned(CurrentAnimation);
  ButtonNewTrack.Enabled := Assigned(CurrentAnimation);
  ComboBoxPlayStyle.Enabled := Assigned(CurrentAnimation);

  if Assigned(CurrentAnimation) then
    FView.Playing := CurrentAnimation.Playing
  else
    FView.Playing := False;
end;

end.
