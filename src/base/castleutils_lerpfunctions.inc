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
function LerpLiner(const ALerp: single): single;
function LerpSin(const ALerp: single): single;
function LerpElastic(const ALerp: single): single;
function LerpBack(const ALerp: single): single;
function LerpOneMinusCos(const ALerp: single): single;
function LerpUniformDeceleration(const ALerp: single): single;

{$endif read_interface}

{$ifdef read_implementation}
function LerpLiner(const ALerp: single): single;
begin
   Result := ALerp;
end;
function LerpSin(const ALerp: single): single;
begin
   Result := Sin(ALerp * Pi/2);
end;
function LerpElastic(const ALerp: single): single;
begin
   Result := (Sin(ALerp * Pi * (0.2 + 2.5 * ALerp * ALerp * ALerp)) *
     Power(1 - ALerp, 2.2) + ALerp) * (1 + (1.2 * (1 - ALerp)));
end;
function LerpBack(const ALerp: single): single;
begin
   Result := ALerp * ALerp * ((2.70158 * ALerp) - 1.70158);
end;
function LerpOneMinusCos(const ALerp: single): single;
begin
   Result := 1 - Cos(ALerp * Pi/2);
end;
function LerpUniformDeceleration(const ALerp: single): single;
begin
   Result := 2 * ALerp - ALerp * ALerp;
end;

{$endif read_implementation}
