unit CastleAnimationPlayer;

interface

uses
  Classes, SysUtils, CastleClassUtils, CastleUtils,
  Generics.Collections, CastleTimeUtils, Rtti, CastleLog;

type
  TAnimationTrackMode = (amDiscrete, amContinuous);

  TAnimationTrack = class
  public
  type
    TAnimationKeyframe = record
      Time: TFloatTime;
      Value: TValue;
    end;

    TAnimationKeyframeList = class(
      {$IFDEF FPC}specialize{$ENDIF} TList<TAnimationKeyframe>)
    end;

  strict private
    FOnChange: TNotifyEvent;
  var
    FComponent: TPersistent;
    FProperty: string;
    FKeyframes: TAnimationKeyframeList;
    FMode: TAnimationTrackMode;
    procedure KeyframesNotify(ASender: TObject; const AItem: TAnimationKeyframe;
      AAction: TCollectionNotification);
    function Interpolate(const Keyframe1, Keyframe2: TAnimationKeyframe;
      const Time: TFloatTime): TValue;
    procedure SetOnChange(const AValue: TNotifyEvent);
  public
    constructor Create(AComponent: TPersistent; const AProperty: string);
    destructor Destroy; override;
    procedure AddKeyframe(const ATime: TFloatTime; const AValue: TValue);
    procedure Evaluate(const ATime: TFloatTime);
    function Duration: TFloatTime;
    function getListLog: string;

    property Mode: TAnimationTrackMode read FMode write FMode;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TAnimationTrackList = class(
    {$IFDEF FPC}specialize{$ENDIF} TObjectList<TAnimationTrack>)
  end;

  TAnimationPlayer = class(TCastleComponent)
  strict private
    FMaxTime: TFloatTime;
    FTrackList: TAnimationTrackList;
    FCurrentTime: TFloatTime;
    FPlaying: boolean;
    FLoop: boolean;
    FSpeed: single;
    procedure FTrackListNotify(ASender: TObject; const AItem: TAnimationTrack;
      AAction: TCollectionNotification);
    procedure SetPlaying(const Value: boolean);
    procedure SetLoop(const Value: boolean);
    procedure SetSpeed(const Value: single);
    function GetMaxTime: TFloatTime;
    procedure Changed;
    procedure TrackChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTrack(const Track: TAnimationTrack);
    procedure RemoveTrack(const Track: TAnimationTrack);
    procedure ClearTracks;
    procedure Update(const DeltaTime: TFloatTime);
    procedure Start;
    procedure Stop;
    function PropertySections(const PropertyName: string): TPropertySections; override;

    property MaxTime: TFloatTime read FMaxTime;
  published
    property Playing: boolean read FPlaying write SetPlaying default False;
    property Loop: boolean read FLoop write SetLoop default False;
    property Speed: single read FSpeed write SetSpeed {$IFDEF FPC}default 1{$ENDIF};

  end;

implementation

uses Math, TypInfo, Generics.Defaults;

function CompareKeyframe(const Left, Right: TAnimationTrack.TAnimationKeyframe): integer;
begin
  Result := CompareValue(Left.Time, Right.Time);
end;

constructor TAnimationTrack.Create(AComponent: TPersistent; const AProperty: string);
type
  TInternalKeyframeComparer =
    {$ifdef FPC}
    specialize
     {$endif}
    TComparer<TAnimationKeyframe>;
begin
  inherited Create;
  FKeyframes := TAnimationKeyframeList.Create(TInternalKeyframeComparer.Construct(
    {$IFDEF FPC}
@
     {$ENDIF}
    CompareKeyframe));
  FKeyframes.OnNotify :=
    {$IFDEF FPC}
   @
     {$ENDIF}
    KeyframesNotify;
  FComponent := AComponent;
  FProperty := AProperty;
end;

destructor TAnimationTrack.Destroy;
begin
  FKeyframes.Free;
  inherited Destroy;
end;

procedure TAnimationTrack.AddKeyframe(const ATime: TFloatTime; const AValue: TValue);
var
  Keyframe: TAnimationKeyframe;
begin
  Keyframe.Time := ATime;
  Keyframe.Value := AValue;
  FKeyframes.Add(Keyframe);
end;

procedure TAnimationTrack.Evaluate(const ATime: TFloatTime);

  procedure SetProperty(const AProperty: string; const AValue: TValue);
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
          on E: Exception do
          begin
            WritelnWarning('TAnimationTrack', 'SetProperty fail');
            WritelnWarning('TAnimationTrack', E.Message);
          end;
        end;
      end;
    end;

  end;

var
  I: integer;
  AValue: TValue;
begin
  if FKeyframes.Count = 0 then
    Exit;

  if ATime < FKeyframes.First.Time then
  begin
    SetProperty(FProperty, FKeyframes.First.Value);
    Exit;
  end;

  {  FKeyframes.BinarySearch(Keyframe, Index);
  if Index < 0 then
    Index := -Index - 1;
  FKeyframes.Insert(Index, Keyframe); }
  for I := 0 to FKeyframes.Count - 2 do
    if (ATime >= FKeyframes[I].Time) and (ATime < FKeyframes[I + 1].Time) then
    begin
      AValue := Interpolate(FKeyframes[I], FKeyframes[I + 1], ATime);
      SetProperty(FProperty, AValue);
      Exit;
    end;

  SetProperty(FProperty, FKeyframes.Last.Value);
end;

function TAnimationTrack.Duration: TFloatTime;
begin
  if FKeyframes.Count < 2 then
    Exit(0);
  Result := FKeyframes.Last.Time - FKeyframes.First.Time;
end;

function TAnimationTrack.getListLog: string;
var
  Frame: TAnimationKeyframe;
begin
  for Frame in self.FKeyframes do
  begin
    Result := Result + Frame.Time.ToString + ', ' + Frame.Value.ToString + sLineBreak;
  end;
end;

procedure TAnimationPlayer.SetPlaying(const Value: boolean);
begin
  if FPlaying <> Value then
  begin
    FPlaying := Value;
    if FPlaying then FCurrentTime := 0;
  end;
end;

procedure TAnimationPlayer.FTrackListNotify(ASender: TObject;
  const AItem: TAnimationTrack; AAction: TCollectionNotification);
begin
  Changed;
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
  FTrackList.OnNotify :=
    {$IFDEF FPC}
    @
     {$ENDIF}
    FTrackListNotify;
  FCurrentTime := 0;
  FMaxTime := 0;
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
  Track.OnChange :=
    {$IFDEF FPC}
    @
     {$ENDIF}
    TrackChange;
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
  if not FPlaying then  Exit;
  if CastleDesignMode then Exit;

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

procedure TAnimationPlayer.Start;
begin
  FCurrentTime := 0;
  FPlaying := True;
end;

function TAnimationPlayer.GetMaxTime: TFloatTime;
var
  I: integer;
  Track: TAnimationTrack;
begin
  Result := 0;
  for I := 0 to FTrackList.Count - 1 do
  begin
    Track := FTrackList[I];
    if Track.Duration > Result then
      Result := Track.Duration;
  end;
end;

procedure TAnimationPlayer.Changed;
begin
  FMaxTime := GetMaxTime;
end;

procedure TAnimationPlayer.TrackChange(Sender: TObject);
begin
  Changed;
end;

function TAnimationTrack.Interpolate(const Keyframe1, Keyframe2: TAnimationKeyframe;
  const Time: TFloatTime): TValue;
var
  Lerp: single;
  V1_int, V2_int: int64;
  V1_float, V2_float: extended;
begin
  if Keyframe1.Value.Kind <> Keyframe2.Value.Kind then
    raise Exception.Create(
      'TAnimationTrack.Interpolate: Interpolation of different value types is not supported.');

  case self.Mode of
    amDiscrete: Result := Keyframe1.Value;
    amContinuous:
    begin
      Lerp := (Time - Keyframe1.Time) / (Keyframe2.Time - Keyframe1.Time);

      if Keyframe1.Value.Kind in [tkInteger, tkEnumeration, tkSet, tkChar, tkWChar] then
      begin
        V1_int := Keyframe1.Value.AsOrdinal;
        V2_int := Keyframe2.Value.AsOrdinal;
        Result := Round((1 - Lerp) * V1_int + Lerp * V2_int);
      end
      else if Keyframe1.Value.Kind in [tkFloat, tkInt64] then
      begin
        V1_float := Keyframe1.Value.AsExtended;
        V2_float := Keyframe2.Value.AsExtended;
        Result := (1 - Lerp) * V1_float + Lerp * V2_float;
      end
      else
        raise Exception.Create('TAnimationTrack.Interpolate: Unsupported value type.');
    end;
  end;

end;

procedure TAnimationTrack.SetOnChange(const AValue: TNotifyEvent);
begin
  if FOnChange = AValue then Exit;
  FOnChange := AValue;
end;

procedure TAnimationTrack.KeyframesNotify(ASender: TObject;
  const AItem: TAnimationKeyframe; AAction: TCollectionNotification);
begin
  FKeyframes.Sort;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TAnimationPlayer.Stop;
begin
  FCurrentTime := 0;
  FPlaying := False;
end;

function TAnimationPlayer.PropertySections(const PropertyName: string): TPropertySections;
begin
  if ArrayContainsString(PropertyName, ['Playing', 'Loop', 'Speed']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;


end.
