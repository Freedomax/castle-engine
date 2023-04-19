  {%MainUnit castlepropedits.pas}
{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

  { TAnimationPlayerEditor ------------------------------------------------- }

type
  TAnimationPlayerEditor = class(TSubPropertiesEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TAnimationPlayerEditor.GetAttributes: TPropertyAttributes;
begin
  Result := (inherited GetAttributes) + [paDialog];
end;

procedure TAnimationPlayerEditor.Edit;
var
  D: TAnimationPlayerDialog;
  comp: TPersistent;
  AnimationPlayer: TAnimationPlayer;
  vClassName: string;
begin
  comp := GetComponent(0);
  if (comp is TCastleAnimationPlayer) then
    AnimationPlayer := (comp as TCastleAnimationPlayer).AnimationPlayer
  else
  if (comp is TCastleAnimationPlayerTransform) then
    AnimationPlayer := (comp as TCastleAnimationPlayerTransform).AnimationPlayer
  else
  begin
    raise Exception.Create(
      'Only support TCastleAnimationPlayer, TCastleAnimationPlayerTransform');
    Exit;
  end;

  D := TAnimationPlayerDialog.GetInstance;
  D.Load(AnimationPlayer);
  D.Show;
  Modified;
end;

