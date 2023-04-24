{%MainUnit castlecontrols.pas}
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
  TAnimationPositionTrack = class(TAnimationVector2Track)
  private
    function GetControl: TCastleUserInterface;
    procedure SetControl(const AValue: TCastleUserInterface);
  strict protected
    procedure SetValue(const AValue: TVector2); override;
  public
    constructor Create(AControl: TCastleUserInterface);
    function ObjectName: string; override;
    function PropName: string; override;
    function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
  published
    property Control: TCastleUserInterface read GetControl write SetControl;
  end;

  TCastleAnimationPlayer = class(TCastleUserInterface)
  strict private
    FAnimationPlayer: TAnimationPlayer;
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
const
  SUserInterface = 'Control';

function TAnimationPositionTrack.GetControl: TCastleUserInterface;
begin
  Result := TCastleUserInterface(FComponent);
end;

procedure TAnimationPositionTrack.SetControl(const AValue: TCastleUserInterface
  );
begin
  SetComponent(FComponent);
end;

{ TAnimationPositionTrack --------------------------------------------------------------- }
procedure TAnimationPositionTrack.SetValue(const AValue: TVector2);
begin
  Control.Translation := AValue;
end;

constructor TAnimationPositionTrack.Create(AControl: TCastleUserInterface);
begin
  inherited Create;
  Control := AControl;
end;

function TAnimationPositionTrack.ObjectName: string;
begin
  if Assigned(Control) then Result := Control.Name
  else Result := '';
end;

function TAnimationPositionTrack.PropName: string;
begin
  Result := 'Translation';
end;

function TAnimationPositionTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(Control) then Exit(inherited);
  AddKeyframe(ATime, Control.Translation, ALerpFunc);
  Result :=True;
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
