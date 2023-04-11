unit CastleAnimationPlayer;

interface

uses
  Classes, SysUtils, CastleClassUtils, CastleUtils,
  Generics.Collections, CastleTimeUtils, Rtti;

type

  TAnimationKeyframe = record
    Time: TFloatTime;
    Value: TValue;
  end;

  TAnimationTrack = class
  private
    FComponent: TCastleComponent;
    FProperty: string;
    FKeyframes: array of TAnimationKeyframe;
    function Interpolate(const Keyframe1, Keyframe2: TAnimationKeyframe;
      const Time: TFloatTime): TValue;
  public
    constructor Create(AComponent: TCastleComponent; const AProperty: string);
    procedure AddKeyframe(Time: TFloatTime; const Value: variant);
    procedure Evaluate(Time: TFloatTime);
    function Duration: TFloatTime;
  end;

  TAnimationTrackList = class(
{$IFDEF FPC}specialize{$ENDIF} TObjectList<TAnimationTrack>)
  end;

  TAnimationPlayer = class(TCastleComponent)
  private
    FTrackList: TAnimationTrackList;
    FCurrentTime: TFloatTime;
    FPlaying: boolean;
    FLoop: boolean;
    FSpeed: single;
    procedure SetPlaying(const Value: boolean);
    procedure SetLoop(const Value: boolean);
    procedure SetSpeed(const Value: single);
    function GetMaxTime: TFloatTime;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTrack(const Track: TAnimationTrack);
    procedure RemoveTrack(const Track: TAnimationTrack);
    procedure ClearTracks;
    procedure Update(const DeltaTime: TFloatTime);
    procedure Stop;
    property Playing: boolean read FPlaying write SetPlaying;
    property Loop: boolean read FLoop write SetLoop;
    property Speed: single read FSpeed write SetSpeed;
    property MaxTime: TFloatTime read GetMaxTime;
  end;

implementation

uses Math, TypInfo;

constructor TAnimationTrack.Create(AComponent: TCastleComponent;
  const AProperty: string);
begin
  inherited Create;
  FComponent := AComponent;
  FProperty := AProperty;
  SetLength(FKeyframes, 0);
end;

procedure TAnimationTrack.AddKeyframe(Time: TFloatTime; const Value: variant);
begin
  SetLength(FKeyframes, Length(FKeyframes) + 1);
  FKeyframes[High(FKeyframes)].Time := Time;
  FKeyframes[High(FKeyframes)].Value := Value;
end;

procedure TAnimationTrack.Evaluate(Time: TFloatTime);

  procedure SetProperty(AProperty: string; AValue: TValue);
  var
    ctx: TRttiContext;
    t: TRttiType;
    p: TRttiProperty;
  begin
    t := ctx.GetType(FComponent.ClassType);
    for p in t.GetProperties do
    begin
      if (p.Visibility = mvPublished) and (p.Name = AProperty) then
      begin
        try
          if p.IsWritable then
          begin
            p.SetValue(FComponent, AValue);
          end;

        except
          on E: Exception do ;
        end;
      end;
    end;

  end;

var
  I: integer;
  AValue: TValue;
begin
  if Length(FKeyframes) = 0 then
    Exit;
  if Time < FKeyframes[0].Time then
  begin
    AValue := FKeyframes[0].Value;
    SetProperty(FProperty, AValue);
    Exit;
  end;
  for I := 0 to High(FKeyframes) - 1 do
    if (Time >= FKeyframes[I].Time) and (Time < FKeyframes[I + 1].Time) then
    begin
      AValue := Interpolate(FKeyframes[I], FKeyframes[I + 1], Time);
      SetProperty(FProperty, AValue);
      Exit;
    end;
  AValue := FKeyframes[High(FKeyframes)].Value;
  SetProperty(FProperty, AValue);
end;

function TAnimationTrack.Duration: TFloatTime;
begin
  if Length(FKeyframes) < 2 then
    Exit(0);
  Result := FKeyframes[High(FKeyframes)].Time - FKeyframes[Low(FKeyframes)].Time;
end;

procedure TAnimationPlayer.SetPlaying(const Value: boolean);
begin
  if FPlaying <> Value then
  begin
    FPlaying := Value;
    if FPlaying then FCurrentTime := 0;
  end;
end;

procedure TAnimationPlayer.SetLoop(const Value: boolean);
begin
  if FLoop <> Value then
  begin
    FLoop := Value;
  end;
end;

procedure TAnimationPlayer.SetSpeed(const Value: single);
begin
  if FSpeed <> Value then
  begin
    FSpeed := Value;
  end;
end;

constructor TAnimationPlayer.Create(AOwner: TComponent);
begin
  inherited;
  FTrackList := TAnimationTrackList.Create(True);
  FCurrentTime := 0;
  FPlaying := False;
  FLoop := False;
  FSpeed := 1;
end;

destructor TAnimationPlayer.Destroy;
begin
  FTrackList.Free;
  inherited;
end;

procedure TAnimationPlayer.AddTrack(const Track: TAnimationTrack);
begin
  FTrackList.Add(Track);
end;

procedure TAnimationPlayer.RemoveTrack(const Track: TAnimationTrack);
begin
  FTrackList.Remove(Track);
end;

procedure TAnimationPlayer.ClearTracks;
begin
  FTrackList.Clear;
end;

procedure TAnimationPlayer.Update(const DeltaTime: TFloatTime);
var
  I: integer;
  Track: TAnimationTrack;
begin
  if FPlaying then
  begin
    FCurrentTime := FCurrentTime + DeltaTime * FSpeed;
    if FLoop then
      FCurrentTime := FCurrentTime mod MaxTime
    else if FCurrentTime > MaxTime then
      Stop;
    for I := 0 to FTrackList.Count - 1 do
    begin
      Track := TAnimationTrack(FTrackList[I]);
      Track.Evaluate(FCurrentTime);
    end;
  end;
end;

function TAnimationPlayer.GetMaxTime: TFloatTime;
var
  I: integer;
  Track: TAnimationTrack;
begin
  Result := 0;
  for I := 0 to FTrackList.Count - 1 do
  begin
    Track := TAnimationTrack(FTrackList[I]);
    if Track.Duration > Result then
      Result := Track.Duration;
  end;
end;

function TAnimationTrack.Interpolate(const Keyframe1, Keyframe2: TAnimationKeyframe;
  const Time: TFloatTime): TValue;
var
  Lerp: single;
  V1_int, V2_int: int64;
  V1_float, V2_float: extended;
begin
  if Keyframe1.Value.Kind <> Keyframe2.Value.Kind then
    raise Exception.Create('Interpolation of different value types is not supported.');

  if Keyframe1.Value.Kind in [tkInteger, tkEnumeration, tkSet, tkChar, tkWChar] then
  begin
    V1_int := Keyframe1.Value.AsOrdinal;
    V2_int := Keyframe2.Value.AsOrdinal;
    Lerp := (Time - Keyframe1.Time) / (Keyframe2.Time - Keyframe1.Time);
    Result := Round((1 - Lerp) * V1_int + Lerp * V2_int);
  end
  else if Keyframe1.Value.Kind in [tkFloat, tkInt64] then
  begin
    V1_float := Keyframe1.Value.AsExtended;
    V2_float := Keyframe2.Value.AsExtended;
    Lerp := (Time - Keyframe1.Time) / (Keyframe2.Time - Keyframe1.Time);
    Result := (1 - Lerp) * V1_float + Lerp * V2_float;
  end
  else
    raise Exception.Create('Unsupported value type.');
end;

procedure TAnimationPlayer.Stop;
begin
  FCurrentTime := 0;
  FPlaying := False;
end;


end.
