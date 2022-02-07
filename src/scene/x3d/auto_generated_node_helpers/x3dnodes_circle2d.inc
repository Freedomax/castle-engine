{ -*- buffer-read-only: t -*-

  Copyright 2015-2021 Michalis Kamburelis.

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
    function GetRadius: Single;
    procedure SetRadius(const Value: Single);
    function GetSlices: Integer;
    procedure SetSlices(const Value: Integer);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdRadius: TSFFloat;
    { X3D field "radius". } { }
    public property FdRadius: TSFFloat read FFdRadius;
    { X3D field "radius". } { }
    property Radius: Single read GetRadius write SetRadius;

    strict private FFdSlices: TSFInt32;
    { X3D field "slices". } { }
    public property FdSlices: TSFInt32 read FFdSlices;
    { X3D field "slices". } { }
    property Slices: Integer read GetSlices write SetSlices;

{$endif read_interface}

{$ifdef read_implementation}

{ TCircle2DNode ----------------------------------------------- }

function TCircle2DNode.GetRadius: Single;
begin
  Result := FdRadius.Value;
end;

procedure TCircle2DNode.SetRadius(const Value: Single);
begin
  FdRadius.Send(Value);
end;

function TCircle2DNode.GetSlices: Integer;
begin
  Result := FdSlices.Value;
end;

procedure TCircle2DNode.SetSlices(const Value: Integer);
begin
  FdSlices.Send(Value);
end;

class function TCircle2DNode.ClassX3DType: String;
begin
  Result := 'Circle2D';
end;

procedure TCircle2DNode.CreateNode;
begin
  inherited;

  FFdRadius := TSFFloat.Create(Self, False, 'radius', 1);
   FdRadius.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdRadius);

  FFdSlices := TSFInt32.Create(Self, True, 'slices', -1);
   FdSlices.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdSlices);
end;

{$endif read_implementation}