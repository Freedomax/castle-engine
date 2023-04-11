unit CastleAnimationPlayer;

interface

uses
  Classes, SysUtils, CastleClassUtils, CastleUtils, Generics.Collections;

type
  TAnimationTrack = class
  private
    FComponent: TCastleComponent;
    FProperty: string;
    FKeyframes: array of record
      Time: single;
      Value: variant;
      end;
  public
    constructor Create(AComponent: TCastleComponent; const AProperty: string);
    procedure AddKeyframe(Time: single; const Value: variant);
    procedure Evaluate(Time: single);
  end;

  TTrackList = class({$IFDEF FPC}specialize{$ENDIF} TObjectList<TAnimationTrack>)
  end;

  TAnimationPlayer = class
  private
    FTrackList: TTrackList;
    FCurrentTime: single;
    FPlaying: boolean;
    FLoop: boolean;
    FSpeed: single;
    procedure SetPlaying(Value: boolean);
    procedure SetLoop(Value: boolean);
    procedure SetSpeed(Value: single);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTrack(Track: TAnimationTrack);
    procedure RemoveTrack(Track: TAnimationTrack);
    procedure ClearTracks;
    procedure Update(DeltaTime: single);
    property Playing: boolean read FPlaying write SetPlaying;
    property Loop: boolean read FLoop write SetLoop;
    property Speed: single read FSpeed write SetSpeed;
  end;

implementation

constructor TAnimationTrack.Create(AComponent: TCastleComponent; const AProperty: string);
begin
  inherited Create;
  FComponent := AComponent;
  FProperty := AProperty;
  SetLength(FKeyframes, 0);
end;

procedure TAnimationTrack.AddKeyframe(Time: single; const Value: variant);
begin
  SetLength(FKeyframes, Length(FKeyframes) + 1);
  FKeyframes[High(FKeyframes)].Time := Time;
  FKeyframes[High(FKeyframes)].Value := Value;
end;

procedure TAnimationTrack.Evaluate(Time: single);
var
  i: integer;
  LerpFactor: single;
begin
  if Length(FKeyframes) = 0 then Exit;
  if Time <= FKeyframes[0].Time then
  begin
    //FComponent.SetPropertyValue(FProperty, FKeyframes[0].Value);
    Exit;
  end;
  if Time >= FKeyframes[High(FKeyframes)].Time then
  begin
    //FComponent.SetPropertyValue(FProperty, FKeyframes[High(FKeyframes)].Value);
    Exit;
  end;
  for i := 0 to High(FKeyframes) - 1 do
  begin
    if Time >= FKeyframes[i].Time then
      if Time <= FKeyframes[i + 1].Time then
      begin
        LerpFactor := (Time - FKeyframes[i].Time) /
          (FKeyframes[i + 1].Time - FKeyframes[i].Time);
        //FComponent.SetPropertyValue(FProperty,
        //  Lerp(FKeyframes[i].Value, FKeyframes[i + 1].Value, LerpFactor));
        Exit;
      end;
  end;
end;

procedure TAnimationPlayer.SetPlaying(Value: boolean);
begin

end;

procedure TAnimationPlayer.SetLoop(Value: boolean);
begin

end;

procedure TAnimationPlayer.SetSpeed(Value: single);
begin

end;

constructor TAnimationPlayer.Create;
begin
  inherited Create;
  FTrackList := TTrackList.Create;
  FCurrentTime := 0;
  FPlaying := False;
  FLoop := False;
  FSpeed := 1;
end;

destructor TAnimationPlayer.Destroy;
begin
end;

procedure TAnimationPlayer.AddTrack(Track: TAnimationTrack);
begin

end;

procedure TAnimationPlayer.RemoveTrack(Track: TAnimationTrack);
begin

end;

procedure TAnimationPlayer.ClearTracks;
begin

end;

procedure TAnimationPlayer.Update(DeltaTime: single);
begin

end;

end.
