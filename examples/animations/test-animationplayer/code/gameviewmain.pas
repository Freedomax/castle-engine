{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleAnimationPlayer,
  CastleTransform, CastleScene, CastleLog, CastleColors;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  private
    procedure Box1AnimationComplete(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps, LabelLog: TCastleLabel;
    Button1: TCastleButton;
    Box1: TCastleBox;
    Sphere1: TCastleSphere;
    AnimationPlayer1: TCastleAnimationPlayer;
    AnimationPlayerTransform1, AnimationPlayerBox1, AnimationPlayerSphere1:
    TCastleAnimationPlayerTransform;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

  TLabelColorTrack = class(TAnimationVector4Track)
  strict private
    FControl: TCastleLabel;
  strict protected
    procedure SetValue(const AValue: variant); override;
  public
    constructor Create(AControl: TCastleLabel);
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

procedure TViewMain.Box1AnimationComplete(Sender: TObject);
var
  AniPlayer: TAnimationPlayer;
begin
  AniPlayer := Sender as TAnimationPlayer;
  LabelLog.Text.Add(Format('Animation: "%s" completed', [AniPlayer.Animation]));
  if AniPlayer.Animation = '1' then AniPlayer.Animation := '2'
  else
  if AniPlayer.Animation = '2' then AniPlayer.Animation := '3';
end;

procedure TViewMain.Button1Click(Sender: TObject);
begin
  AnimationPlayerTransform1.AnimationPlayer.Start;
end;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

function UniformDecelerationFunc(const ALerp: single): single;
begin
  Result := 2 * ALerp - ALerp * ALerp;
end;

function UniformCircularFunc_OneMiuCos(const ALerp: single): single;
begin
  Result := 1 - Cos(ALerp * Pi / 2);
end;

function UniformCircularFunc_Sin(const ALerp: single): single;
begin
  Result := Sin(ALerp * Pi / 2);
end;

procedure TViewMain.Start;
var
  Animation: TAnimation;
  Track: TAnimationTrack;
  TranslationTrack: TAnimationTranslationTrack;
  PositionTrack: TAnimationPositionTrack;
  RotationTrack: TAnimationRotationTrack;
  LabelColorTrack: TLabelColorTrack;
begin
  inherited;
  Button1.OnClick := Button1Click;

  { Box }
  //1
  Animation := AnimationPlayerBox1.AnimationPlayer.NewAnimation('1');
  TranslationTrack := TAnimationTranslationTrack.Create(
    (AnimationPlayerBox1.Parent as TCastleBox));
  TranslationTrack.Mode := tmContinuous;
  TranslationTrack.AddKeyframe(4, Vector3(3.5, 1, -3));
  TranslationTrack.AddKeyframe(0, Vector3(-4.0, 1, -3));
  TranslationTrack.AddKeyframe(2, Vector3(1.0, 1, -3),
 {$Ifdef fpc}@{$endif}UniformDecelerationFunc);

  Animation.AddTrack(TranslationTrack);
  //2
  Animation := AnimationPlayerBox1.AnimationPlayer.NewAnimation('2');
  Track := TAnimationPropertyTrack.Create(
    (AnimationPlayerBox1.Parent as TCastleBox).RotationPersistent, 'W');
  Track.Mode := tmContinuous;
  Track.AddKeyframe(0, 0.0);
  Track.AddKeyframe(1, -Pi / 2);
  Animation.AddTrack(Track);
  //3
  Animation := AnimationPlayerBox1.AnimationPlayer.NewAnimation('3');
  Track := TAnimationPropertyTrack.Create(
    (AnimationPlayerBox1.Parent as TCastleBox).TranslationPersistent, 'Z');
  Track.Mode := tmContinuous;
  Track.AddKeyframe(0, -3.0);
  Track.AddKeyframe(2, 2.0, UniformDecelerationFunc);
  Track.AddKeyframe(4, 4.0);
  Animation.PlayStyle := apsPingPongOnce;
  Animation.AddTrack(Track);
  //Play 1
  AnimationPlayerBox1.AnimationPlayer.OnAnimationComplete := Box1AnimationComplete;
  AnimationPlayerBox1.AnimationPlayer.Animation := '1';
  AnimationPlayerBox1.AnimationPlayer.Playing := True;

  { Label }
  Animation := TAnimation.Create;

  LabelColorTrack := TLabelColorTrack.Create(AnimationPlayer1.Parent as TCastleLabel);
  LabelColorTrack.Mode := tmContinuous;
  LabelColorTrack.AddKeyframe(1, Vector4(1, 0.3, 0.2, 1));
  LabelColorTrack.AddKeyframe(0, Vector4(1, 0.3, 0.2, 1));
  LabelColorTrack.AddKeyframe(0.25, Vector4(0.2, 0.3, 0.2, 0.6));
  LabelColorTrack.AddKeyframe(0.5, Vector4(1, 0.3, 0.2, 0.2));

  Animation.AddTrack(LabelColorTrack);

  PositionTrack := TAnimationPositionTrack.Create(AnimationPlayer1.Parent as
    TCastleLabel);
  PositionTrack.Mode := tmContinuous;
  PositionTrack.AddKeyframe(0, Vector2(20, 60));
  PositionTrack.AddKeyframe(0.5, Vector2(27, 71));
  PositionTrack.AddKeyframe(1, Vector2(20, 60));
  Animation.AddTrack(PositionTrack);

  Track := TAnimationPropertyTrack.Create(
    (AnimationPlayer1.Parent as TCastleLabel), 'Caption');
  Track.Mode := tmDiscrete;
  Track.AddKeyframe(0, 'Click <font color="#00FF00FF">Button</font> to shake camera');
  Track.AddKeyframe(0.5, 'Hello world');
  Track.AddKeyframe(1, '');
  Animation.AddTrack(Track);

  Animation.PlayStyle := apsLoop;
  Animation.Speed := 0.5;
  AnimationPlayer1.AnimationPlayer.AddAnimation('1', Animation);
  AnimationPlayer1.AnimationPlayer.Animation := '1';
  AnimationPlayer1.AnimationPlayer.Playing := True;

  { Sphere }
  Animation := AnimationPlayerSphere1.AnimationPlayer.NewAnimation('1');

  Track := TAnimationPropertyTrack.Create(
    (AnimationPlayerSphere1.Parent as TCastleSphere).TranslationPersistent, 'X');
  Track.Mode := tmContinuous;
  Track.AddKeyframe(1, 2.0);
  Track.AddKeyframe(0, -2.0, UniformCircularFunc_OneMiuCos);
  Track.AddKeyframe(0.5, 0.0, UniformCircularFunc_Sin);
  Animation.AddTrack(Track);

  Track := TAnimationPropertyTrack.Create(
    (AnimationPlayerSphere1.Parent as TCastleSphere).TranslationPersistent, 'Z');
  Track.Mode := tmContinuous;
  Track.AddKeyframe(1, -2.0);
  Track.AddKeyframe(0, -2.0, UniformCircularFunc_Sin);
  Track.AddKeyframe(0.5, 0.0, UniformCircularFunc_OneMiuCos);
  Animation.AddTrack(Track);

  Animation.PlayStyle := apsPingPong;
  Animation.Speed := 0.5;
  AnimationPlayerSphere1.AnimationPlayer.Start;
  AnimationPlayerSphere1.AnimationPlayer.Animation := '1';
  { Camera }
  Animation := AnimationPlayerTransform1.AnimationPlayer.NewAnimation('1');

  RotationTrack := TAnimationRotationTrack.Create(AnimationPlayerTransform1.Parent as
    TCastleCamera);
  RotationTrack.Mode := tmContinuous;
  RotationTrack.AddKeyframe(0, Vector4(-1, 0, 0, 1.3));
  RotationTrack.AddKeyframe(1 / 3, Vector4(-1, 0.05, 0.04, 1.25));
  RotationTrack.AddKeyframe(0.5, Vector4(-1, 0, 0, 1.2));
  RotationTrack.AddKeyframe(2 / 3, Vector4(-1, -0.05, -0.03, 1.25));
  RotationTrack.AddKeyframe(1, Vector4(-1, 0, 0, 1.3));
  Animation.AddTrack(RotationTrack);

  Animation.PlayStyle := apsOnce;
  Animation.Speed := 5;
  AnimationPlayerTransform1.AnimationPlayer.Playing := False;
  AnimationPlayerTransform1.AnimationPlayer.Animation := '1';

end;

procedure TViewMain.Update(const SecondsPassed: single; var HandleInput: boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil,
    'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

end;

procedure TLabelColorTrack.SetValue(const AValue: variant);
begin
  FControl.Color := VariantToVector4(AValue);
end;

constructor TLabelColorTrack.Create(AControl: TCastleLabel);
begin
  inherited Create;
  FControl := AControl;
end;

end.
