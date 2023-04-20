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
  if AniPlayer.Animation = 'box1' then AniPlayer.Animation := 'box2'
  else
  if AniPlayer.Animation = 'box2' then AniPlayer.Animation := 'box3';
end;

procedure TViewMain.Button1Click(Sender: TObject);
begin
  AnimationPlayerTransform1.AnimationPlayer.Animation := '1';
  AnimationPlayerTransform1.AnimationPlayer.Start;
end;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  Button1.OnClick := Button1Click;

  AnimationPlayerBox1.AnimationPlayer.OnAnimationComplete := Box1AnimationComplete;

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
