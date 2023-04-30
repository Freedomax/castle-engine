{%MainUnit castletransform.pas}
{
  Copyright 2023 Michalis Kamburelis, Freedomax.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TAnimationTranslationTrack = class(TAnimationVector3Track)
  private
    function GetTransform: TCastleTransform;
    procedure SetTransform(const AValue: TCastleTransform);
  protected
    procedure SetValue(const AValue: TVector3); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function PropName: string; override;
    function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
  published
    property Transform: TCastleTransform read GetTransform write SetTransform;
  end;

  TAnimationRotationTrack = class(TAnimationVector4Track)
  private
    function GetTransform: TCastleTransform;
    procedure SetTransform(const AValue: TCastleTransform);
  protected
    procedure SetValue(const AValue: TVector4); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function PropName: string; override;
    function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
  published
    property Transform: TCastleTransform read GetTransform write SetTransform;
  end;

  TAnimationScaleTrack = class(TAnimationVector3Track)
  private
    function GetTransform: TCastleTransform;
    procedure SetTransform(const AValue: TCastleTransform);
  protected
    procedure SetValue(const AValue: TVector3); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function PropName: string; override;
    function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
  published
    property Transform: TCastleTransform read GetTransform write SetTransform;
  end;

  TCastleAnimationPlayerTransform = class(TCastleTransform)
  strict private
    FAnimationPlayer: TAnimationPlayer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property AnimationPlayer: TAnimationPlayer read FAnimationPlayer;
  end;

{$endif read_interface}

{$ifdef read_implementation}
function TAnimationTranslationTrack.GetTransform: TCastleTransform;
begin
  Result := TCastleTransform(FComponent);
end;

procedure TAnimationTranslationTrack.SetTransform(const AValue: TCastleTransform
  );
begin
  SetComponent(AValue);
end;

{ TAnimationTranslationTrack --------------------------------------------------------------- }
procedure TAnimationTranslationTrack.SetValue(const AValue: TVector3);
begin
  Transform.Translation := AValue;
end;

constructor TAnimationTranslationTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  Transform := ATransform;
end;

function TAnimationTranslationTrack.PropName: string;
begin
  Result := 'Translation';
end;

function TAnimationTranslationTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(Transform) then Exit(inherited);
  AddKeyframe(ATime, Transform.Translation, ALerpFunc);
  Result :=True;
end;

function TAnimationRotationTrack.GetTransform: TCastleTransform;
begin
  Result := TCastleTransform(FComponent);
end;

procedure TAnimationRotationTrack.SetTransform(const AValue: TCastleTransform);
begin
  SetComponent(AValue);
end;

{ TAnimationRotationTrack --------------------------------------------------------------- }
procedure TAnimationRotationTrack.SetValue(const AValue: TVector4);
begin
  if Assigned(Transform) then
    Transform.Rotation := AValue;
end;

constructor TAnimationRotationTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  Transform := ATransform;
end;

function TAnimationRotationTrack.PropName: string;
begin
  Result := 'Rotation';
end;

function TAnimationRotationTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(Transform) then Exit(inherited);
  AddKeyframe(ATime, Transform.Rotation, ALerpFunc);
  Result :=True;
end;

function TAnimationScaleTrack.GetTransform: TCastleTransform;
begin
  Result := TCastleTransform(FComponent);
end;

procedure TAnimationScaleTrack.SetTransform(const AValue: TCastleTransform);
begin
    SetComponent(AValue);
end;

{ TAnimationScaleTrack --------------------------------------------------------------- }
procedure TAnimationScaleTrack.SetValue(const AValue: TVector3);
begin
  Transform.Scale := AValue;
end;

constructor TAnimationScaleTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  Transform := ATransform;
end;

function TAnimationScaleTrack.PropName: string;
begin
  Result := 'Scale';
end;

function TAnimationScaleTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(Transform) then Exit(inherited);
  AddKeyframe(ATime, Transform.Scale, ALerpFunc);
  Result :=True;
end;

{ TCastleAnimationPlayerTransform --------------------------------------------------------------- }
constructor TCastleAnimationPlayerTransform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimationPlayer := TAnimationPlayer.Create(Self);
  FAnimationPlayer.SetSubComponent(True);
end;

procedure TCastleAnimationPlayerTransform.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  FAnimationPlayer.Update(SecondsPassed);
end;

function TCastleAnimationPlayerTransform.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, ['AnimationPlayer']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
