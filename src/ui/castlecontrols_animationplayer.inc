{%MainUnit castlecontrols.pas}
{
  Copyright 2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TAnimationPositionTrack = class(TAnimationVector2Track)
  strict private
    FControl: TCastleUserInterface;
  strict protected
    procedure SetValue(const AValue: variant); override;
  public
    constructor Create(AControl: TCastleUserInterface);
  end;

  TCastleAnimationPlayer = class(TCastleUserInterface)
  strict private
    FAnimationPlayer: TAnimationPlayer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property AnimationPlayer: TAnimationPlayer read FAnimationPlayer;
  end;

{$endif read_interface}

{$ifdef read_implementation}
{ TAnimationPositionTrack --------------------------------------------------------------- }
procedure TAnimationPositionTrack.SetValue(const AValue: variant);
begin
  FControl.Translation := VariantToVector2(AValue);
end;

constructor TAnimationPositionTrack.Create(AControl: TCastleUserInterface);
begin
  inherited Create;
  FControl := AControl;
end;

{ TCastleAnimationPlayer --------------------------------------------------------------- }
constructor TCastleAnimationPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimationPlayer := TAnimationPlayer.Create(Self);
  FAnimationPlayer.SetSubComponent(True);
end;

procedure TCastleAnimationPlayer.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  FAnimationPlayer.Update(SecondsPassed);
end;

function TCastleAnimationPlayer.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, ['AnimationPlayer']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
