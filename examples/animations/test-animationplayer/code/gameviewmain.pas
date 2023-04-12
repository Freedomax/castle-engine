{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleAnimationPlayer,
  CastleTransform, CastleScene, CastleLog;

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
    AnimationPlayer1: TCastleAnimationPlayer;
    AnimationPlayerTransform1, AnimationPlayerBox1: TCastleAnimationPlayerTransform;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): boolean; override;
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
  LabelLog.Text.Add('Animation: "%s" completed', [AniPlayer.Animation]);
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

procedure TViewMain.Start;
var
  Animation: TAnimation;
  Track: TAnimationTrack;
  AIndex: SizeInt;
begin
  inherited;
  Button1.OnClick :=
    {$IFDEF FPC}
  @
     {$ENDIF}
    Button1Click;
  ;
  { Box }
  //1
  Animation := TAnimation.Create;
  Track := TAnimationTrack.Create(
    (AnimationPlayerBox1.Parent as TCastleBox).TranslationPersistent, 'X');
  Track.Mode := amContinuous;
  Track.AddKeyframe(0, -4.0);
  Track.AddKeyframe(2, 1.0,
    {$IFDEF FPC}
  @
     {$ENDIF}
    UniformDecelerationFunc);
  Track.AddKeyframe(4, 3.5);
  Animation.AddTrack(Track);
  AnimationPlayerBox1.AnimationPlayer.AddAnimation('1', Animation);
  //2
  Animation := TAnimation.Create;
  Track := TAnimationTrack.Create(
    (AnimationPlayerBox1.Parent as TCastleBox).RotationPersistent, 'W');
  Track.Mode := amContinuous;
  Track.AddKeyframe(0, 0.0);
  Track.AddKeyframe(1, -Pi / 2);
  Animation.AddTrack(Track);
  AnimationPlayerBox1.AnimationPlayer.AddAnimation('2', Animation);
  //3
  Animation := TAnimation.Create;
  Track := TAnimationTrack.Create(
    (AnimationPlayerBox1.Parent as TCastleBox).TranslationPersistent, 'Z');
  Track.Mode := amContinuous;
  Track.AddKeyframe(0, -3.0);
  Track.AddKeyframe(2, 2.0,
    {$IFDEF FPC}
  @
     {$ENDIF}
    UniformDecelerationFunc);
  Track.AddKeyframe(4, 4.0);
  Animation.AddTrack(Track);
  AnimationPlayerBox1.AnimationPlayer.AddAnimation('3', Animation);
  //Play 1
  AnimationPlayerBox1.AnimationPlayer.OnAnimationComplete :=
    {$IFDEF FPC}
     @
     {$ENDIF}
    Box1AnimationComplete;
  AnimationPlayerBox1.AnimationPlayer.Animation := '1';
  AnimationPlayerBox1.AnimationPlayer.Playing := True;


  { UI }
  Animation := TAnimation.Create;

  Track := TAnimationTrack.Create(
    (AnimationPlayer1.Parent as TCastleLabel).ColorPersistent, 'Alpha');
  Track.Mode := amContinuous;
  Track.AddKeyframe(0.5, 0.2);
  Track.AddKeyframe(1, 1.0);
  Track.AddKeyframe(0, 1.0);
  Animation.AddTrack(Track);


  Track := TAnimationTrack.Create(
    (AnimationPlayer1.Parent as TCastleLabel).ColorPersistent, 'Red');
  Track.Mode := amContinuous;
  Track.AddKeyframe(0, 1.0);
  Track.AddKeyframe(0.25, 0.2);
  Track.AddKeyframe(0.5, 1.0);
  Animation.AddTrack(Track);

  Track := TAnimationTrack.Create(
    (AnimationPlayer1.Parent as TCastleLabel).TranslationPersistent, 'X');
  Track.Mode := amContinuous;
  Track.AddKeyframe(0, 20);
  Track.AddKeyframe(0.5, 27);
  Track.AddKeyframe(1, 20);
  Animation.AddTrack(Track);

  Track := TAnimationTrack.Create(
    (AnimationPlayer1.Parent as TCastleLabel).TranslationPersistent, 'Y');
  Track.Mode := amContinuous;
  Track.AddKeyframe(0, 60);
  Track.AddKeyframe(0.5, 71);
  Track.AddKeyframe(1, 60);
  Animation.AddTrack(Track);

  Track := TAnimationTrack.Create((AnimationPlayer1.Parent as TCastleLabel),
    'Caption');
  Track.Mode := amDiscrete;
  Track.AddKeyframe(0, 'Click <font color="#00FF00FF">Button</font> to shake camera');
  Track.AddKeyframe(0.5, 'Hello world');
  Track.AddKeyframe(1, 'Click <font color="#00FF00FF">Button</font> to shake camera');
  Animation.AddTrack(Track);

  Animation.Loop := True;
  Animation.Speed := 0.5;
  AnimationPlayer1.AnimationPlayer.AddAnimation('1', Animation);
  AnimationPlayer1.AnimationPlayer.Animation := '1';
  AnimationPlayer1.AnimationPlayer.Playing := True;

  { Camera }
  Animation := TAnimation.Create;

  Track := TAnimationTrack.Create(
    (AnimationPlayerTransform1.Parent as TCastleCamera).RotationPersistent, 'W');
  Track.Mode := amContinuous;
  Track.AddKeyframe(0, 1.3);
  Track.AddKeyframe(0.5, 1.2);
  Track.AddKeyframe(1, 1.3);
  Animation.AddTrack(Track);

  Track := TAnimationTrack.Create(
    (AnimationPlayerTransform1.Parent as TCastleCamera).RotationPersistent, 'Y');
  Track.Mode := amContinuous;
  Track.AddKeyframe(0, 0.0);
  Track.AddKeyframe(1 / 3, 0.1);
  Track.AddKeyframe(2 / 3, -0.1);
  Track.AddKeyframe(1, 0.0);
  Animation.AddTrack(Track);

  Track := TAnimationTrack.Create(
    (AnimationPlayerTransform1.Parent as TCastleCamera).RotationPersistent, 'Z');
  Track.Mode := amContinuous;
  Track.AddKeyframe(0, 0.0);
  Track.AddKeyframe(1 / 3, 0.11);
  Track.AddKeyframe(2 / 3, -0.11);
  Track.AddKeyframe(1, 0.0);
  Animation.AddTrack(Track);

  Animation.Loop := False;
  Animation.Speed := 5;
  AnimationPlayerTransform1.AnimationPlayer.Playing := False;
  AnimationPlayerTransform1.AnimationPlayer.AddAnimation('1', Animation);
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

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

end.
