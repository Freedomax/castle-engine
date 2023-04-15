{
  Copyright 2002-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
{   TLerpFuncType = (lftCustom, lftLiner, lftSin, lftCos, lftOneMinusSin, lftOneMinusCos,
    lftUniformDeceleration);        }
function LerpFuncLiner(const ALerp: single): single;
function LerpFuncSin(const ALerp: single): single;
function LerpFuncCos(const ALerp: single): single;
function LerpFuncOneMinusSin(const ALerp: single): single;
function LerpFuncOneMinusCos(const ALerp: single): single;
function LerpFuncUniformDeceleration(const ALerp: single): single;

{$endif read_interface}

{$ifdef read_implementation}
function LerpFuncLiner(const ALerp: single): single;
begin
   Result := ALerp;
end;
function LerpFuncSin(const ALerp: single): single;
begin
   Result := Sin(ALerp * Pi/2);
end;
function LerpFuncCos(const ALerp: single): single;
begin
   Result := Cos(ALerp * Pi/2);
end;
function LerpFuncOneMinusSin(const ALerp: single): single;
begin
   Result := 1 - Sin(ALerp * Pi/2);
end;
function LerpFuncOneMinusCos(const ALerp: single): single;
begin
   Result := 1 - Cos(ALerp * Pi/2);
end;
function LerpFuncUniformDeceleration(const ALerp: single): single;
begin
   Result := 2 * ALerp - ALerp * ALerp;
end;

{$endif read_implementation}
