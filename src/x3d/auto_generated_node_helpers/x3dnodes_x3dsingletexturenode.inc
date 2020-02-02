{ -*- buffer-read-only: t -*-

  Copyright 2015-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatically generated node properties.

  Do not edit this file manually!
  To add new properties:
  - add them to the text files in tools/internal/x3d-nodes-to-pascal/nodes-specification/ ,
  - and regenerate include files by running x3d-nodes-to-pascal .

  The documentation for properties should go to x3dnodes_documentation.txt . }

{$ifdef read_interface}

  strict private
{$ifndef CASTLE_SLIM_NODES}
    function GetCrossOrigin: string;
    procedure SetCrossOrigin(const Value: string);
{$endif not CASTLE_SLIM_NODES}
  public
    { Set X3D field "effects". } { }
    procedure SetEffects(const Value: array of TEffectNode);
{$ifndef CASTLE_SLIM_NODES}
    { X3D field "crossOrigin". } { }
    property CrossOrigin: string read GetCrossOrigin write SetCrossOrigin;
{$endif not CASTLE_SLIM_NODES}

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractSingleTextureNode ----------------------------------------------- }

procedure TAbstractSingleTextureNode.SetEffects(const Value: array of TEffectNode);
var
  L: Integer;
  A: array of TX3DNode;
begin
  L := High(Value) + 1;
  SetLength(A, L);
  if L > 0 then
    Move(Value[0], A[0], L * SizeOf(TEffectNode));
  FdEffects.Send(A);
end;

{$ifndef CASTLE_SLIM_NODES}
function TAbstractSingleTextureNode.GetCrossOrigin: string;
begin
  Result := FdCrossOrigin.Value;
end;

procedure TAbstractSingleTextureNode.SetCrossOrigin(const Value: string);
begin
  FdCrossOrigin.Send(Value);
end;

{$endif not CASTLE_SLIM_NODES}
{$endif read_implementation}