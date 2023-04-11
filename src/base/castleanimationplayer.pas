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
      {$IFDEF FPC}specialize{$ENDIF} TSortedList<TAnimationKeyframe>)
    public
      function SearchIndex(const AValue: TAnimationKeyframe): SizeInt;

    end;

  strict private
    FOnChange: TNotifyEvent;
  var
    FComponent: TPersistent;
    FProperty: string;
    FKeyframes: TAnimationKeyframeList;
    FMode: TAnimationTrackMode;
    procedure KeyframesNotify(ASender: TObject; {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationKeyframe;
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

    property Mode: TAnimationTrackMode read FMode write FMode;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TAnimationTrackList = class(
    {$IFDEF FPC}specialize{$ENDIF} TObjectList<TAnimationTrack>)
  end;

  TAnimation = class
  strict private
    FMaxTime: TFloatTime;
    FTrackList: TAnimationTrackList;
    FCurrentTime: TFloatTime;
    FPlaying: boolean;
    FLoop: boolean;
    FSpeed: single;
    procedure FTrackListNotify(ASender: TObject; {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationTrack;
      AAction: TCollectionNotification);
    procedure SetPlaying(const Value: boolean);
    procedure SetLoop(const Value: boolean);
    procedure SetSpeed(const Value: single);
    function GetMaxTime: TFloatTime;
    procedure Changed;
    procedure TrackChange(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTrack(const Track: TAnimationTrack);
    procedure RemoveTrack(const Track: TAnimationTrack);
    procedure ClearTracks;
    procedure Update(const DeltaTime: TFloatTime);
    procedure Start(const ResetTime: boolean = True);
    procedure Stop(const ResetTime: boolean = True);
    property MaxTime: TFloatTime read FMaxTime;

    property Loop: boolean read FLoop write SetLoop default False;
    property Speed: single read FSpeed write SetSpeed {$IFDEF FPC}default 1{$ENDIF};
    property Playing: boolean read FPlaying write SetPlaying default False;
  end;

  TAnimationList = {$IFDEF FPC}
    specialize
  {$ENDIF}  TObjectDictionary<string, TAnimation>;


  TAnimationPlayer = class(TCastleComponent)
  private
    FAnimation: string;
    FCurrentAnimation: TAnimation;
    FAnimationList: TAnimationList;
    FPlaying: boolean;
    procedure UpdateAnimation;
    procedure SetAnimation(const AValue: string);
    procedure SetPlaying(const AValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: string): TPropertySections; override;
    procedure Update(const DeltaTime: TFloatTime);
    procedure AddAnimation(const AName: string; const AAnimation: TAnimation);
    procedure RemoveAnimation(const AName: string);
    procedure ClearAnimations;
    procedure Start(const ResetTime: boolean = True);
    procedure Stop(const ResetTime: boolean = True);

    property Animation: string read FAnimation write SetAnimation;
  published

    property Playing: boolean read FPlaying write SetPlaying default False;
  end;

implementation

uses Math, TypInfo, Generics.Defaults, Generics.Strings;

function CompareKeyframe({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} Left, Right: TAnimationTrack.TAnimationKeyframe): integer;
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
  AValue: TValue;
  Index: SizeInt;
  Keyframe: TAnimationKeyframe;
begin
  if FKeyframes.Count = 0 then
    Exit;

  if ATime < FKeyframes.First.Time then
    AValue := FKeyframes.First.Value
  else
  begin
    Keyframe.Time := ATime;
    Index := FKeyframes.SearchIndex(Keyframe) - 1;
    if Between(Index, 0, FKeyframes.Count - 2) then
      AValue := Interpolate(FKeyframes[Index], FKeyframes[Index + 1], ATime)
    else
      AValue := FKeyframes.Last.Value;
  end;
  SetProperty(FProperty, AValue);
end;

function TAnimationTrack.Duration: TFloatTime;
begin
  if FKeyframes.Count < 2 then
    Exit(0);
  Result := FKeyframes.Last.Time - FKeyframes.First.Time;
end;

procedure TAnimation.SetPlaying(const Value: boolean);
begin
  if FPlaying <> Value then
  begin
    FPlaying := Value;
    if FPlaying then FCurrentTime := 0;
  end;
end;

procedure TAnimation.FTrackListNotify(ASender: TObject;
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationTrack; AAction: TCollectionNotification);
begin
  Changed;
end;

procedure TAnimation.SetLoop(const Value: boolean);
begin
  if FLoop <> Value then
  begin
    FLoop := Value;
  end;
end;

procedure TAnimation.SetSpeed(const Value: single);
begin
  if FSpeed <> Value then
  begin
    FSpeed := Value;
  end;
end;

constructor TAnimation.Create;
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

destructor TAnimation.Destroy;
begin
  FTrackList.Free;
  inherited;
end;

procedure TAnimation.AddTrack(const Track: TAnimationTrack);
begin
  FTrackList.Add(Track);
  Track.OnChange :=
    {$IFDEF FPC}
    @
     {$ENDIF}
    TrackChange;
end;

procedure TAnimation.RemoveTrack(const Track: TAnimationTrack);
begin
  FTrackList.Remove(Track);
end;

procedure TAnimation.ClearTracks;
begin
  FTrackList.Clear;
end;

procedure TAnimation.Update(const DeltaTime: TFloatTime);
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

procedure TAnimation.Start(const ResetTime: boolean);
begin
  if ResetTime then
    FCurrentTime := 0;
  FPlaying := True;
end;

function TAnimation.GetMaxTime: TFloatTime;
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

procedure TAnimation.Changed;
begin
  FMaxTime := GetMaxTime;
end;

procedure TAnimation.TrackChange(Sender: TObject);
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
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationKeyframe; AAction: TCollectionNotification);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TAnimationTrack.TAnimationKeyframeList.SearchIndex(
  const AValue: TAnimationKeyframe): SizeInt;
var
  LSearchResult: TBinarySearchResult;
begin
  if
  {$IFDEF FPC}
   specialize
   {$ENDIF}
  TArrayHelper<TAnimationKeyframe>.BinarySearch(FItems, AValue,
    LSearchResult, FComparer, 0, Count) then
    case FDuplicates of
      dupAccept: Result := LSearchResult.FoundIndex;
      dupIgnore: Exit(LSearchResult.FoundIndex);
      dupError: raise EListError.Create(SCollectionDuplicate);
    end
  else
  begin
    if LSearchResult.CandidateIndex = -1 then
      Result := 0
    else
    if LSearchResult.CompareResult > 0 then
      Result := LSearchResult.CandidateIndex
    else
      Result := LSearchResult.CandidateIndex + 1;
  end;
end;

procedure TAnimation.Stop(const ResetTime: boolean);
begin
  if ResetTime then
    FCurrentTime := 0;
  FPlaying := False;
end;

procedure TAnimationPlayer.UpdateAnimation;
begin
  if not Assigned(FCurrentAnimation) then exit;
  if FPlaying then FCurrentAnimation.Start(True)
  else
    FCurrentAnimation.Stop(True);
end;

procedure TAnimationPlayer.SetAnimation(const AValue: string);
begin
  if FAnimation = AValue then Exit;
  FAnimation := AValue;

  FCurrentAnimation := nil;
  if not FAnimationList.TryGetValue(FAnimation, FCurrentAnimation) then
  begin
    FAnimation := '';
    WritelnWarning('AnimationPlayer', 'Animation "%s" not exists', [AValue]);
  end;

  UpdateAnimation;
end;

procedure TAnimationPlayer.SetPlaying(const AValue: boolean);
begin
  if FPlaying = AValue then Exit;

  FPlaying := AValue;
  UpdateAnimation;
end;

constructor TAnimationPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimationList := TAnimationList.Create([doOwnsValues]);
end;

destructor TAnimationPlayer.Destroy;
begin
  FAnimationList.Free;
  inherited Destroy;
end;

function TAnimationPlayer.PropertySections(const PropertyName: string): TPropertySections;
begin
  if ArrayContainsString(PropertyName, ['Playing', 'Animation']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TAnimationPlayer.Update(const DeltaTime: TFloatTime);
begin
  if not FPlaying then Exit;
  if not Assigned(FCurrentAnimation) then Exit;

  FCurrentAnimation.Update(DeltaTime);
end;

procedure TAnimationPlayer.AddAnimation(const AName: string;
  const AAnimation: TAnimation);
begin
  if AName = '' then
    raise Exception.Create('AnimationPlayer: Name must not be empty');

  if not Assigned(AAnimation) then
    raise Exception.Create('AnimationPlayer: TAnimation is nil');

  if not FAnimationList.TryAdd(AName, AAnimation) then
    raise Exception.Create('AnimationPlayer: AddAnimation fail');
end;

procedure TAnimationPlayer.RemoveAnimation(const AName: string);
begin
  if FAnimation = AName then
    Animation := '';

  FAnimationList.Remove(AName);
end;

procedure TAnimationPlayer.ClearAnimations;
begin
  FAnimationList.Clear;
end;

procedure TAnimationPlayer.Start(const ResetTime: boolean);
begin
  if not FPlaying then
    FPlaying := True;
  if Assigned(FCurrentAnimation) then FCurrentAnimation.Start(ResetTime);
end;

procedure TAnimationPlayer.Stop(const ResetTime: boolean);
begin
  if FPlaying then
    FPlaying := False;
  if Assigned(FCurrentAnimation) then FCurrentAnimation.Stop(ResetTime);
end;


end.
