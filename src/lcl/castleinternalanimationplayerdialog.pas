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
  CastleViewport, CastleTimeUtils, Variants, CastleRectangles, CastleFonts;

type
  TTrackView = class(TCastleRectangleControl)
  private
  var
    FTrack: TAnimationTrack;
    procedure SetTrack(const AValue: TAnimationTrack);
  public
    function GetMousePosTime(const APixelsPerSceond: single): TFloatTime;
    function GetMousePosFrame(const APixelsPerSceond: single): integer;
    function UnderMouse: boolean;
    property Track: TAnimationTrack read FTrack write SetTrack;
  end;

  TTrackListScrollView = class(TCastleScrollView)
  end;

  TTrackDesignerUI = class(TCastleUserInterface)
  private
    FButtonRemove, FButtonLerpFunc: TCastleButton;
    FTimeDragControl: TCastleRectangleControl;
    FUIContainer: TCastleVerticalGroup;
  public
  const
    DragUIHeight = 20;
    ButtonHeight = 20;
    ButtonFontSize = 12;
    constructor Create(AOwner: TComponent); override;
    property ButtonRemove: TCastleButton read FButtonRemove;
    property ButtonLerpFunc: TCastleButton read FButtonLerpFunc;
    property TimeDragControl: TCastleRectangleControl read FTimeDragControl;
  end;

  TTrackViewList = class( {$Ifdef fpc}specialize{$endif} TObjectList<TTrackView>)
  public
    function FocusedTrackView: TTrackView;
  end;

  TAnimationPlayerView = class(TCastleView)
  private
    FAnimationPlayerChanged: TNotifyEvent;
    FCurrentAnimationChanged: TNotifyEvent;
    FPixelsPerSceond: single;
    FPlaying: boolean;
    FPlayingChanged: TNotifyEvent;
  const
    TrackHeight = 100;
    TrackHeadViewWidth = 150;
    ItemFontSize = 15;
    ItemFontSmallSize = 12;
    ItemSpacing = 2;
    TrackListHeadHeight = 20;
  var
    FAnimationPlayer: TAnimationPlayer;
    FRoot: TCastleUserInterface;
    FTrackListView: TCastleVerticalGroup;
    FTrackViewList: TTrackViewList;
    procedure AButtonDeleteClick(Sender: TObject);
    procedure ACheckBoxChange(Sender: TObject);
    procedure AddKeyFrameButtonClick(Sender: TObject);
    procedure AScrollViewHeaderPress(const Sender: TCastleUserInterface;
      const Event: TInputPressRelease; var Handled: boolean);
    procedure AScrollViewHeaderRender(const Sender: TCastleUserInterface);
    procedure ATrackViewRender(const Sender: TCastleUserInterface);
    procedure FAnimationPlayerAnimationComplete(Sender: TObject);
    procedure FAnimationPlayerCurrentAnimationChanged(Sender: TObject);
    function GetCurrentTime: TFloatTime;
    { Track index, "-1" means all changed. }
    procedure KeyFrameListChanged(const Index: integer);
    function GetCurrentAnimation: TAnimation;
    procedure SetAnimationPlayer(const AValue: TAnimationPlayer);
    procedure SetAnimationPlayerChanged(const AValue: TNotifyEvent);
    procedure SetCurrentAnimationChanged(const AValue: TNotifyEvent);
    procedure SetCurrentTime(const AValue: TFloatTime);
    procedure SetPixelsPerSceond(const AValue: single);
    procedure SetPlaying(const AValue: boolean);
    procedure SetPlayingChanged(const AValue: TNotifyEvent);

  protected
    FFont: TCastleFont;
    ButtonAddKeyFrame: TCastleButton;
    TrackDesignerUI: TTrackDesignerUI;
    procedure ReloadTracks;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function MousePosToTime(const AMousePos: TVector2): TFloatTime;

    property CurrentTime: TFloatTime read GetCurrentTime write SetCurrentTime;
  public
    constructor Create(AOwner: TComponent); override;
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
    property PixelsPerSceond: single read FPixelsPerSceond write SetPixelsPerSceond;
    property PlayingChanged: TNotifyEvent read FPlayingChanged write SetPlayingChanged;
    property AnimationPlayerChanged: TNotifyEvent
      read FAnimationPlayerChanged write SetAnimationPlayerChanged;
    property CurrentAnimationChanged: TNotifyEvent
      read FCurrentAnimationChanged write SetCurrentAnimationChanged;
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
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel1: TPanel;
    PopupMenuAddTrack: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    procedure ButtonRemoveAnimationClick(Sender: TObject);
    procedure ButtonNewAnimationClick(Sender: TObject);
    procedure ButtonNewTrackClick(Sender: TObject);
    procedure ButtonPlayStopClick(Sender: TObject);
    procedure ComboBoxAnimationChange(Sender: TObject);
    procedure ComboBoxPlayStyleChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
  private
    FView: TAnimationPlayerView;

    procedure FViewAnimationPlayerChanged(Sender: TObject);
    procedure FViewCurrentAnimationChanged(Sender: TObject);
    procedure FViewPlayingChanged(Sender: TObject);
    function GetAnimationPlayer: TAnimationPlayer;
    function GetPlayerParentUI: TCastleUserInterface;
    function SelectTransform: TCastleTransform;
    function SelectUI: TCastleUserInterface;

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
    { Get glocal instance. }
    class function GetInstance: TAnimationPlayerDialog;
    destructor Destroy; override;
    procedure Load(const AAnimationPlayer: TAnimationPlayer);
  end;

implementation

uses Math,
  CastleLclUtils, CastleInternalPropertySelectDialog, CastleGLUtils,
  CastleRenderOptions, TypInfo, CastleStringUtils, CastleUtils;

{$R *.lfm}

procedure TTrackView.SetTrack(const AValue: TAnimationTrack);
begin
  if FTrack <> AValue then
  begin
    FTrack := AValue;
  end;
end;

function TTrackView.GetMousePosTime(const APixelsPerSceond: single): TFloatTime;
var
  AMousePos: TVector2;
begin
  AMousePos := ContainerToLocalPosition(Container.MousePosition);
  Result := AMousePos.X / APixelsPerSceond;
  if Result < 0 then Result := 0;
end;

function TTrackView.GetMousePosFrame(const APixelsPerSceond: single): integer;
begin
  Result := FTrack.KeyframeList.TimeToKeyFrame(GetMousePosTime(APixelsPerSceond));
end;

function TTrackView.UnderMouse: boolean;
var
  R: TFloatRectangle;
begin
  if not Assigned(Container) then Exit(False);
  { Extending the selection range to the entire window allows the last frame to be selected as well. }
  R := RenderRect;
  if R.Right < Container.Width then
    R.Width := R.Width + Container.Width - R.Right;
  Result := R.Contains(Container.MousePosition);
end;

constructor TTrackDesignerUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 60;
  AutoSizeToChildren := True;
  FUIContainer := TCastleVerticalGroup.Create(Self);
  FUIContainer.AutoSizeToChildren := True;
  InsertFront(FUIContainer);

  FTimeDragControl := TCastleRectangleControl.Create(Self);
  FTimeDragControl.Width := Width;
  FTimeDragControl.Height := DragUIHeight;
  FTimeDragControl.Color := Vector4(1, 1, 1, 0.5);
  FUIContainer.InsertFront(FTimeDragControl);

  FButtonLerpFunc := TCastleButton.Create(Self);
  FButtonLerpFunc.AutoSize := False;
  FButtonLerpFunc.Width := Width;
  FButtonLerpFunc.Height := ButtonHeight;
  FButtonLerpFunc.Caption := 'LerpFunc';
  FButtonLerpFunc.FontSize := ButtonFontSize;
  FUIContainer.InsertFront(FButtonLerpFunc);

  FButtonRemove := TCastleButton.Create(Self);
  FButtonRemove.AutoSize := False;
  FButtonRemove.Width := Width;
  FButtonRemove.Height := ButtonHeight;
  FButtonRemove.Caption := 'Remove';
  FButtonRemove.FontSize := ButtonFontSize;
  FUIContainer.InsertFront(FButtonRemove);
end;

function TTrackViewList.FocusedTrackView: TTrackView;
var
  ATrackView: TTrackView;
begin
  Result := nil;
  //TDO: optimize.
  for ATrackView in self do
  begin
    if ATrackView.UnderMouse then exit(ATrackView);
  end;
end;

procedure TAnimationPlayerView.SetAnimationPlayer(const AValue: TAnimationPlayer);
begin
  if FAnimationPlayer <> AValue then
  begin
    if Assigned(FAnimationPlayer) then
    begin
      FAnimationPlayer.OnAnimationComplete := nil;
      FAnimationPlayer.OnCurrentAnimationChanged := nil;
      FAnimationPlayer.RemoveFreeNotification(Self);
    end;
    FAnimationPlayer := AValue;
    if Assigned(FAnimationPlayer) then
    begin
      FAnimationPlayer.OnAnimationComplete :=
 {$Ifdef fpc}@{$endif}FAnimationPlayerAnimationComplete;
      FAnimationPlayer.OnCurrentAnimationChanged :=
 {$Ifdef fpc}@{$endif}FAnimationPlayerCurrentAnimationChanged;
      FAnimationPlayer.FreeNotification(Self);
    end;
    if Assigned(FAnimationPlayerChanged) then FAnimationPlayerChanged(Self);
  end;

end;

procedure TAnimationPlayerView.SetAnimationPlayerChanged(const AValue: TNotifyEvent);
begin
  if not SameMethods(TMethod(FAnimationPlayerChanged), TMethod(AValue)) then
    FAnimationPlayerChanged := AValue;
end;

procedure TAnimationPlayerView.SetCurrentAnimationChanged(const AValue: TNotifyEvent);
begin
  if not SameMethods(TMethod(FCurrentAnimationChanged), TMethod(AValue)) then
    FCurrentAnimationChanged := AValue;
end;

procedure TAnimationPlayerView.SetCurrentTime(const AValue: TFloatTime);
begin
  if Assigned(CurrentAnimation) then
  begin
    CurrentAnimation.ActualCurrentTime := AValue;
  end;
end;

procedure TAnimationPlayerView.SetPixelsPerSceond(const AValue: single);
begin
  if FPixelsPerSceond = AValue then Exit;
  FPixelsPerSceond := AValue;
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

    if FPlaying and Assigned(AnimationPlayer) and not AnimationPlayer.Playing then
      ShowMessage(
        'Unable to play currently, please first set AnimationPlayer.Playing = true in object inspector.');
  end;
end;

procedure TAnimationPlayerView.SetPlayingChanged(const AValue: TNotifyEvent);
begin
  if FPlayingChanged <> AValue then
    FPlayingChanged := AValue;
end;

procedure TAnimationPlayerView.AddKeyFrameButtonClick(Sender: TObject);

  function GetMousePosTime: TFloatTime;
  var
    ATrackView: TTrackView;
  begin
    ATrackView := FTrackViewList.First;
    Result := ATrackView.GetMousePosTime(PixelsPerSceond);
  end;

var
  ATime: TFloatTime;
  Track: TAnimationTrack;
  v: variant;
  slt: TStringList;
begin
  if not Assigned(CurrentAnimation) then Exit;
  if CurrentAnimation.TrackList.Count = 0 then Exit;

  slt := TStringList.Create;
  try
    ATime := GetMousePosTime;
    for Track in CurrentAnimation.TrackList do
    begin
      if not Track.AddKeyframeAtTime(ATime) then
        slt.Add('AddKeyframeAtTime fail: ' + Track.ClassName);
    end;

    KeyFrameListChanged(-1);
    if slt.Count > 0 then ShowMessage(slt.Text);
  finally
    FreeAndNil(slt);
  end;

end;

procedure TAnimationPlayerView.AScrollViewHeaderPress(
  const Sender: TCastleUserInterface;
  const Event: TInputPressRelease; var Handled: boolean);
begin
  if Event.IsMouseButton(TCastleMouseButton.buttonLeft) then
  begin
    if Assigned(CurrentAnimation) then
    begin
      CurrentTime := MousePosToTime(Event.Position);
      CurrentAnimation.ForceUpdate;
    end;
  end;
end;

procedure TAnimationPlayerView.AScrollViewHeaderRender(
  const Sender: TCastleUserInterface);
var
  HeaderView: TCastleUserInterfaceFont;

  procedure RenderPlaybackLine;
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
    if not Assigned(CurrentAnimation) then Exit;
    if FTrackViewList.Count = 0 then Exit;
    if not Playing then Exit;

    RTrack := FTrackViewList.First.RenderRect;
    R := RenderRect;
    RenderLine(RTrack.Left + PixelsPerSceond * CurrentAnimation.ActualCurrentTime *
      UIScale, CastleColors.Green, 2);
  end;

  procedure RenderTimeLine(const R: TFloatRectangle; const Scale: single);

    procedure RenderLine(const P1, P2: TVector2; const LineColor: TCastleColor;
    const LineWidth: single);
    begin
      DrawPrimitive2D(pmLines,
        [P1, P2],
        LineColor, bsSrcAlpha, bdOneMinusSrcAlpha, False, LineWidth);
    end;

  var
    X, Y2, Y1, Y2Long, Y2Middle: single;
    I: integer;
    DeltaTime: TFloatTime;
    Str: string;
    DeltaW: single;
  begin
    Y1 := R.Top;
    Y2Long := R.Bottom + 2 * HeaderView.UIScale;
    Y2Middle := (R.Top + R.Bottom) / 2;
    DeltaTime := 0.1;
    DeltaW := DeltaTime * PixelsPerSceond * HeaderView.UIScale * Scale;
    for I := 0 to Trunc(R.Width / DeltaW) - 1 do
    begin
      X := R.Left + (TrackHeadViewWidth + ItemSpacing) * UIScale + I * DeltaW;
      if (I mod 10) = 0 then
      begin
        Str := FormatFloat('0.#', I * DeltaTime);
        HeaderView.Font.Print(Vector2(X + 2 * HeaderView.UIScale, Y2Long),
          Vector4(1, 1, 1, 0.88), Str);
      end;
      if (I mod 5) = 0 then Y2 := Y2Long
      else
        Y2 := Y2Middle;
      RenderLine(Vector2(X, Y1), Vector2(X, Y2), Vector4(1, 1, 1, 0.5), Scale * 2);
    end;
  end;

var
  RHeader: TFloatRectangle;
begin
  { Draw head rect. }
  HeaderView := (Sender as TCastleUserInterfaceFont);
  RHeader := HeaderView.RenderRect;
  DrawRectangle(RHeader, Vector4(0, 0, 0, 0.618));
  { TimeLine. }
  RenderTimeLine(RHeader, 1);
  { PlaybackLine. }
  RenderPlaybackLine;
end;

procedure TAnimationPlayerView.ATrackViewRender(const Sender: TCastleUserInterface);
var
  ATrackView: TTrackView;
  FramePos: single;
  R: TFloatRectangle;

  function TimeRenderPosition(const ATime: TFloatTime): single;
  begin
    Result := R.Left + ATime * PixelsPerSceond * UIScale;
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

  procedure RenderKeyFrame(const bSelected: boolean = False);
  var
    Rc: TFloatRectangle;
  begin
    RenderLine(FramePos, CastleColors.White, 1 * UIScale);

    if bSelected then
    begin
      rc := FloatRectangle(FramePos - 10 * UIScale, R.Bottom, 20 * UIScale, R.Height);
      DrawRectangle(Rc, Vector4(1, 1, 1, 0.382));

    end;
    //RenderDetail;
  end;

var
  AIndex, I: integer;
begin
  ATrackView := Sender as TTrackView;
  AIndex := ATrackView.GetMousePosFrame(PixelsPerSceond);
  R := ATrackView.RenderRect;
  for I := 0 to ATrackView.Track.KeyframeList.Count - 1 do
  begin
    FramePos := TimeRenderPosition(ATrackView.Track.KeyframeList[i].Time);
    RenderKeyFrame(I = AIndex);
  end;
end;

procedure TAnimationPlayerView.FAnimationPlayerAnimationComplete(Sender: TObject);
begin
  Playing := False;
end;

procedure TAnimationPlayerView.FAnimationPlayerCurrentAnimationChanged(
  Sender: TObject);
begin
  if Assigned(FCurrentAnimationChanged) then FCurrentAnimationChanged(Self);
end;

function TAnimationPlayerView.GetCurrentTime: TFloatTime;
begin
  if Assigned(CurrentAnimation) then Result := CurrentAnimation.ActualCurrentTime
  else
    Result := -1;
end;

procedure TAnimationPlayerView.KeyFrameListChanged(const Index: integer);

  procedure FixSize(const AIndex: integer);
  begin
    FTrackViewList.Items[AIndex].Width :=
      Max(10, PixelsPerSceond * CurrentAnimation.TrackList.Items[AIndex].Duration);
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
      raise Exception.Create('argument out of range.');
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
  ATrack: TAnimationTrack;
  ATrackContainer: TCastleHorizontalGroup;
  ATrackView: TTrackView;
  ATrackHeadView: TCastleRectangleControl;
  { TrackHeadView items. }
  AHeadItemContainer: TCastleVerticalGroup;
  ACheckBox: TCastleCheckbox;
  ALabelPropName: TCastleLabel;
  ALabelObjectName: TCastleLabel;
  AButtonDelete: TCastleButton;
begin
  FTrackListView.ClearControls;
  FTrackViewList.Clear;
  if not Assigned(CurrentAnimation) then
  begin
    ButtonAddKeyFrame.Exists := False;
    Exit;
  end;

  ButtonAddKeyFrame.Exists := CurrentAnimation.TrackList.Count > 0;
  for  I := 0 to CurrentAnimation.TrackList.Count - 1 do
  begin
    ATrack := CurrentAnimation.TrackList.Items[I];
    ATrackContainer := TCastleHorizontalGroup.Create(self);
    ATrackContainer.Spacing := ItemSpacing;
    ATrackContainer.Culling := True;
    FTrackListView.InsertFront(ATrackContainer);

    ATrackHeadView := TCastleRectangleControl.Create(Self);
    ATrackHeadView.Height := TrackHeight;
    ATrackHeadView.Color := Vector4(0, 0, 0, 0.4);
    ATrackHeadView.Tag := I;
    ATrackHeadView.Width := TrackHeadViewWidth;
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
    ATrackView.OnRender := {$Ifdef fpc}@{$endif}ATrackViewRender;
    ATrackContainer.InsertFront(ATrackView);
    FTrackViewList.Add(ATrackView);
  end;
  KeyFrameListChanged(-1);
end;

procedure TAnimationPlayerView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = AnimationPlayer) then
    AnimationPlayer := nil;
end;

function TAnimationPlayerView.MousePosToTime(const AMousePos: TVector2): TFloatTime;
var
  Pos: TVector2;
begin
  Pos := ContainerToLocalPosition(AMousePos);
  Result := (Pos.X - (TrackHeadViewWidth + ItemSpacing)) / PixelsPerSceond;

end;

constructor TAnimationPlayerView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPixelsPerSceond := 200;
end;

{ TAnimationPlayerView ---------------------------------------------------- }
procedure TAnimationPlayerView.Start;
var
  AScrollView: TTrackListScrollView;
  AScrollViewHeader: TCastleUserInterfaceFont;
begin
  inherited Start;
  FFont := TCastleFont.Create(Self);

  FTrackViewList := TTrackViewList.Create(False);

  FRoot := TCastleUserInterface.Create(self);
  FRoot.FullSize := True;
  self.InsertFront(FRoot);

  AScrollView := TTrackListScrollView.Create(Self);
  FRoot.InsertFront(AScrollView);
  AScrollView.FullSize := True;
  AScrollView.EnableDragging := True;

  AScrollViewHeader := TCastleUserInterfaceFont.Create(Self);
  AScrollViewHeader.Anchor(vpTop, vpTop);
  AScrollViewHeader.Anchor(hpLeft, hpLeft);
  AScrollViewHeader.FontSize := Self.ItemFontSize;
  AScrollView.InsertFront(AScrollViewHeader);
  AScrollViewHeader.Height := TrackListHeadHeight;
  AScrollViewHeader.WidthFraction := 1.0;
  AScrollViewHeader.OnRender := {$Ifdef fpc}@{$endif}AScrollViewHeaderRender;
  AScrollViewHeader.OnPress := {$Ifdef fpc}@{$endif}AScrollViewHeaderPress;

  FTrackListView := TCastleVerticalGroup.Create(self);
  FTrackListView.FullSize := False;
  FTrackListView.Spacing := ItemSpacing;
  FTrackListView.AutoSizeToChildren := True;
  FTrackListView.Anchor(vpTop, vpTop);
  FTrackListView.Anchor(hpLeft, hpLeft);
  FTrackListView.Translation := Vector2(0, -TrackListHeadHeight);
  AScrollView.ScrollArea.InsertFront(FTrackListView);

  ButtonAddKeyFrame := TCastleButton.Create(self);
  ButtonAddKeyFrame.OnClick :={$IFDEF FPC}@{$ENDIF}AddKeyFrameButtonClick;
  ButtonAddKeyFrame.Caption := '+';
  ButtonAddKeyFrame.Anchor(vpTop, vpTop);
  ButtonAddKeyFrame.Anchor(hpLeft, hpLeft);
  ButtonAddKeyFrame.AutoSize := False;
  ButtonAddKeyFrame.Height := TrackListHeadHeight;
  ButtonAddKeyFrame.Width := TrackListHeadHeight;
  FRoot.InsertFront(ButtonAddKeyFrame);

  TrackDesignerUI := TTrackDesignerUI.Create(Self);
  TrackDesignerUI.Height := TrackHeight;
  TrackDesignerUI.Exists := False;
  FRoot.InsertFront(TrackDesignerUI);
end;

procedure TAnimationPlayerView.Stop;
begin
  FreeAndNil(FTrackViewList);
  inherited stop;
end;

procedure TAnimationPlayerView.RenderOverChildren;
begin
  inherited RenderOverChildren;
end;

procedure TAnimationPlayerView.Update(const SecondsPassed: single;
  var HandleInput: boolean);
var
  ATrackView: TTrackView;
  V: TVector2;
  AIndex: integer;
begin
  inherited Update(SecondsPassed, HandleInput);

  ButtonAddKeyFrame.Translation :=
    Vector2(ContainerToLocalPosition(Container.MousePosition).X -
    ButtonAddKeyFrame.EffectiveWidth / 2, -TrackListHeadHeight);

  ATrackView := FTrackViewList.FocusedTrackView;
  if Assigned(ATrackView) then
  begin
    AIndex := ATrackView.GetMousePosFrame(PixelsPerSceond);
    TrackDesignerUI.Exists :=
      Between(AIndex, 0, ATrackView.Track.KeyframeList.Count - 1);
    if TrackDesignerUI.Exists then
    begin
      V := ATrackView.LocalToContainerPosition(
        Vector2(ATrackView.Track.KeyframeList[AIndex].Time *
        PixelsPerSceond, 0), True);
      V := self.ContainerToLocalPosition(V);
      TrackDesignerUI.Translation := V;
    end;
  end
  else
    TrackDesignerUI.Exists := False;
end;

function TAnimationPlayerView.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited Press(Event);
end;

function TAnimationPlayerView.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited Motion(Event);
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

var
  AnimationPlayerDialog: TAnimationPlayerDialog;

class function TAnimationPlayerDialog.GetInstance: TAnimationPlayerDialog;
begin
  if not Assigned(AnimationPlayerDialog) then
    AnimationPlayerDialog := TAnimationPlayerDialog.Create(Application);
  Result := AnimationPlayerDialog;
end;

destructor TAnimationPlayerDialog.Destroy;
begin
  inherited;
end;

procedure TAnimationPlayerDialog.Load(const AAnimationPlayer: TAnimationPlayer);
begin
  FView.AnimationPlayer := AAnimationPlayer;
end;

procedure TAnimationPlayerDialog.MenuItem1Click(Sender: TObject);
var
  Form: TPropertySelectForm;
  Track: TAnimationPropertyTrack;
begin
  Form := TPropertySelectForm.Create(nil);
  try
    Form.Load(GetPlayerParentUI, False);

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
        ShowMessage('Did not complete the selection.');
    end;
  finally
    FreeAndNil(Form);
  end;
end;

procedure TAnimationPlayerDialog.MenuItem2Click(Sender: TObject);
var
  i: integer;
  Track: TAnimationPropertyTrack;
  AObject: TPersistent;
  APropName, AFriendlyObjectName: string;

  function GetObject: TPersistent;
  var
    comp: TComponent;
  begin
    comp := AnimationPlayer.Owner;
    if (comp is TCastleUserInterface) then
    begin
      Result := (comp as TCastleUserInterface).Parent.TranslationPersistent;
      AFriendlyObjectName :=
        (comp as TCastleUserInterface).Parent.Name + '.TranslationPersistent';
    end
    else
    begin
      Result := (comp as TCastleTransform).Parent.TranslationPersistent;
      AFriendlyObjectName :=
        (comp as TCastleTransform).Parent.Name + '.TranslationPersistent';
    end;
  end;

begin
  for i := 0 to 9 do
  begin
    if i mod 2 = 0 then APropName := 'X'
    else
      APropName := 'Y';
    AObject := GetObject;
    Track := TAnimationPropertyTrack.Create(AObject, APropName);

    Track.FriendlyObjectName := AFriendlyObjectName;
    FView.AddTrack(Track);
  end;
end;

procedure TAnimationPlayerDialog.MenuItem3Click(Sender: TObject);
var
  Track: TAnimationTranslationTrack;
  T: TCastleTransform;
begin
  T := SelectTransform;
  if not Assigned(T) then Exit;

  Track := TAnimationTranslationTrack.Create(T);
  FView.AddTrack(Track);
end;

procedure TAnimationPlayerDialog.MenuItem4Click(Sender: TObject);
var
  Track: TAnimationPositionTrack;
  AControl: TCastleUserInterface;
begin
  AControl := SelectUI;
  if not Assigned(AControl) then Exit;

  Track := TAnimationPositionTrack.Create(AControl);
  FView.AddTrack(Track);
end;

procedure TAnimationPlayerDialog.MenuItem5Click(Sender: TObject);
var
  Track: TAnimationRotationTrack;
  T: TCastleTransform;
begin
  T := SelectTransform;
  if not Assigned(T) then Exit;

  Track := TAnimationRotationTrack.Create(T);
  FView.AddTrack(Track);
end;

procedure TAnimationPlayerDialog.MenuItem6Click(Sender: TObject);
var
  Track: TAnimationScaleTrack;
  T: TCastleTransform;
begin
  T := SelectTransform;
  if not Assigned(T) then Exit;

  Track := TAnimationScaleTrack.Create(T);
  FView.AddTrack(Track);
end;

function TAnimationPlayerDialog.GetCurrentAnimation: TAnimation;
begin
  Result := FView.CurrentAnimation;
end;

function TAnimationPlayerDialog.GetAnimationPlayer: TAnimationPlayer;
begin
  Result := FView.AnimationPlayer;
end;

function TAnimationPlayerDialog.GetPlayerParentUI: TCastleUserInterface;
var
  comp: TComponent;
begin
  comp := FView.AnimationPlayer.Owner;
  if comp is TCastleUserInterface then
    Result := (comp as TCastleUserInterface)
  else
  if comp is TCastleTransform then
    Result := ((comp as TCastleTransform).World.Owner as TCastleUserInterface)
  else
    raise Exception.Create('Cannot find root TCastleUserInterface');
end;

function TAnimationPlayerDialog.SelectTransform: TCastleTransform;
var
  Form: TPropertySelectForm;
begin
  Result := nil;
  Form := TPropertySelectForm.Create(nil);
  try
    Form.Load(GetPlayerParentUI, False, psmComponent);

    if Form.ShowModal = mrOk then
    begin
      if Assigned(Form.SelectResult.SelectedObject) and
        (Form.SelectResult.SelectedObject is TCastleTransform) then
      begin
        Result := Form.SelectResult.SelectedObject as TCastleTransform;
      end
      else
        ShowMessage('Did not complete the selection.');
    end;
  finally
    FreeAndNil(Form);
  end;
end;

function TAnimationPlayerDialog.SelectUI: TCastleUserInterface;
var
  Form: TPropertySelectForm;
begin
  Result := nil;
  Form := TPropertySelectForm.Create(nil);
  try
    Form.Load(GetPlayerParentUI, False, psmComponent);

    if Form.ShowModal = mrOk then
    begin
      if Assigned(Form.SelectResult.SelectedObject) and
        (Form.SelectResult.SelectedObject is TCastleUserInterface) then
      begin
        Result := Form.SelectResult.SelectedObject as TCastleUserInterface;
      end
      else
        ShowMessage('Did not complete the selection.');
    end;
  finally
    FreeAndNil(Form);
  end;
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

procedure TAnimationPlayerDialog.FViewAnimationPlayerChanged(Sender: TObject);
var
  AName: string;
const
  ACaption = 'Castle Animation Player';
begin
  if Assigned(AnimationPlayer) then AName := (AnimationPlayer.Owner as TComponent).Name
  else
    AName := '';
  if Aname = '' then
    Caption := ACaption
  else
    Caption := ACaption + ' - ' + AName;
  AnimationListChanged;
end;

procedure TAnimationPlayerDialog.FViewCurrentAnimationChanged(Sender: TObject);
begin
  AnimationListChanged;
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
var
  AName: string;
begin
  AName := AnimationPlayer.Animation;
  if MessageDlg('Confirm', Format('Are you sure you want to delete "%s"?', [AName]),
    TMsgDlgType.mtConfirmation, [mbOK, mbCancel], '') = mrCancel then
    Exit;
  AnimationPlayer.RemoveAnimation(AName);
  AnimationListChanged;
end;

procedure TAnimationPlayerDialog.InitView;
begin
  if not Assigned(FView) then
  begin
    FView := TAnimationPlayerView.Create(CastleControl1);
    FView.PlayingChanged := {$Ifdef fpc}@{$endif}FViewPlayingChanged;
    FView.AnimationPlayerChanged := {$Ifdef fpc}@{$endif}FViewAnimationPlayerChanged;
    FView.CurrentAnimationChanged := {$Ifdef fpc}@{$endif}FViewCurrentAnimationChanged;
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
    if Assigned(AnimationPlayer) then
    begin
      for AName in AnimationPlayer.AnimationList.Keys do
        ComboBoxAnimation.Items.Add(AName);

      ComboBoxAnimation.ItemIndex :=
        ComboBoxAnimation.Items.IndexOf(AnimationPlayer.Animation);
    end;

  finally
    ComboBoxAnimation.Items.EndUpdate;
  end;
  CurrentAnimationChanged;

end;

procedure TAnimationPlayerDialog.CurrentAnimationChanged;
begin
  UpdateUIControls;
  FView.ReloadTracks;
end;

procedure TAnimationPlayerDialog.UpdateUIControls;
begin
  ComboBoxAnimation.Enabled := Assigned(AnimationPlayer);
  ButtonPlayStop.Enabled := Assigned(CurrentAnimation);
  ButtonNewAnimation.Enabled := Assigned(AnimationPlayer);
  ButtonRemoveAnimation.Enabled := Assigned(CurrentAnimation);
  ButtonNewTrack.Enabled := Assigned(CurrentAnimation);
  ComboBoxPlayStyle.Enabled := Assigned(CurrentAnimation);
  if Assigned(CurrentAnimation) then
    ComboBoxPlayStyle.ItemIndex := Ord(AnimationPlayer.CurrentAnimation.PlayStyle);
  if Assigned(CurrentAnimation) then
    FView.Playing := CurrentAnimation.Playing
  else
    FView.Playing := False;
end;

end.
