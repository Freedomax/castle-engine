{%MainUnit castleanimationplayer.pas}
{
  Copyright 2002-2021 Michalis Kamburelis, FreeDomax.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

function LerpLiner(const ALerp: single): single;
function LerpSin(const ALerp: single): single;
function LerpElastic(const ALerp: single): single;
function LerpBack(const ALerp: single): single;
function LerpOneMinusCos(const ALerp: single): single;
function LerpUniformDeceleration(const ALerp: single): single;

//https://github.com/WladiD/AnyiQuack/wiki/Reference-of-simple-AnyiQuack-types#tfunction
function LerpQuad(const ALerp: single): single;
function LerpCubic(const ALerp: single): single;
function LerpLowWave(ALerp: Real): Real;
function LerpMiddleWave(const ALerp: Single): Single;
function LerpHighWave(const ALerp: Single): Single;
function LerpBounce(const ALerp: Single): Single;
function LerpCircle(const ALerp: Single): Single;

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

function LerpQuad(const ALerp: single): single;
begin
  Result := ALerp * ALerp;
end;

function LerpCubic(const ALerp: single): single;
begin
  Result := Power(ALerp, 3);
end;

function LerpLowWave(const ALerp: Single): Single;
begin
  Result := ALerp + (Sin(ALerp * 3 * Pi) * 0.1);
end;

function LerpMiddleWave(const ALerp: Single): Single;
begin
  Result := ALerp + (Sin(ALerp * 3 * Pi) * 0.2);
end;

function LerpHighWave(const ALerp: Single): Single;
begin
  Result := ALerp + (Sin(ALerp * 3 * Pi) * 0.4);
end;

function LerpBounce(const ALerp: Single): Single;
const
  Base: Single = 7.5625;
var
  f: Single;
begin
  if ALerp < (1 / 2.75) then
    Result := Base * ALerp * ALerp
  else if ALerp < (2 / 2.75) then
  begin
    f := ALerp - (1.5 / 2.75);
    Result := (Base * f) * f + 0.75;
  end
  else if ALerp < (2.5 / 2.75) then
  begin
    f := ALerp - (2.25 / 2.75);
    Result := (Base * f) * f + 0.9375;
  end
  else
  begin
    f := ALerp - (2.625 / 2.75);
    Result := (Base * f) * f + 0.984375;
  end;
end;

function LerpCircle(const ALerp: Single): Single;
begin
  Result := 1 - Sqrt(1 - ALerp * ALerp);
end;

{$endif read_implementation}
