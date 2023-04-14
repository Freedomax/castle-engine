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
  strict private
    FTransform: TCastleTransform;
  strict protected
    procedure SetValue(const AValue: variant); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function ObjectName: string; override;
    function PropName: string; override;
  end;

  TAnimationRotationTrack = class(TAnimationVector4Track)
  strict private
    FTransform: TCastleTransform;
  strict protected
    procedure SetValue(const AValue: variant); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function ObjectName: string; override;
    function PropName: string; override;
  end;

  TAnimationScaleTrack = class(TAnimationVector3Track)
  strict private
    FTransform: TCastleTransform;
  strict protected
    procedure SetValue(const AValue: variant); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function ObjectName: string; override;
    function PropName: string; override;
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
{ TAnimationTranslationTrack --------------------------------------------------------------- }
procedure TAnimationTranslationTrack.SetValue(const AValue: variant);
begin
  FTransform.Translation := VariantToVector3(AValue);
end;

constructor TAnimationTranslationTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  FTransform := ATransform;
end;

function TAnimationTranslationTrack.ObjectName: string;
begin
  if Assigned(FTransform) then Result := FTransform.Name
  else
    Result := inherited;
end;

function TAnimationTranslationTrack.PropName: string;
begin
  Result := 'Translation';
end;

{ TAnimationRotationTrack --------------------------------------------------------------- }
procedure TAnimationRotationTrack.SetValue(const AValue: variant);
begin
  FTransform.Rotation := VariantToVector4(AValue);
end;

constructor TAnimationRotationTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  FTransform := ATransform;
end;

function TAnimationRotationTrack.ObjectName: string;
begin
  if Assigned(FTransform) then Result := FTransform.Name
  else Result := inherited;
end;

function TAnimationRotationTrack.PropName: string;
begin
  Result := 'Rotation';
end;

{ TAnimationScaleTrack --------------------------------------------------------------- }
procedure TAnimationScaleTrack.SetValue(const AValue: variant);
begin
  FTransform.Scale := VariantToVector3(AValue);
end;

constructor TAnimationScaleTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  FTransform := ATransform;
end;

function TAnimationScaleTrack.ObjectName: string;
begin
  if Assigned(FTransform) then Result := FTransform.Name
  else Result := inherited;
end;

function TAnimationScaleTrack.PropName: string;
begin
  Result := 'Scale';
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
