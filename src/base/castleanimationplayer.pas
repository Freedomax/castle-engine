{
  Copyright 2023 Michalis Kamburelis, Freedomax.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit CastleAnimationPlayer;

{$I castleconf.inc}
interface

uses
  Classes, SysUtils, CastleClassUtils, CastleUtils,
  Generics.Collections, CastleTimeUtils, CastleLog, TypInfo, Variants, CastleVectors;

type
  TAnimationTrackMode = (amDiscrete, amContinuous);
  TLerpFunc = function(const ALerp: single): single;

  TAnimationTrack = class abstract
  public
  type
    TAnimationKeyframe = record
      Time: TFloatTime;
      Value: variant;
      LerpFunc: TLerpFunc;
    end;

    TAnimationKeyframeList = class(
      {$IFDEF FPC}specialize{$ENDIF} TSortedList<TAnimationKeyframe>)
    protected
      function SearchIndex(const AValue: TAnimationKeyframe): SizeInt;

    end;

  strict private
    FOnChange: TNotifyEvent;
    FKeyframes: TAnimationKeyframeList;
    FMode: TAnimationTrackMode;
    procedure KeyframesNotify(ASender: TObject;
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationKeyframe;
      AAction: TCollectionNotification);
    function Interpolate(const Keyframe1, Keyframe2: TAnimationKeyframe;
      const Time: TFloatTime): variant;
    procedure SetOnChange(const AValue: TNotifyEvent);
  strict protected
    procedure SetValue(const AValue: variant); virtual;abstract;
    function CalcValue(const Value1, Value2: variant; const ALerp: Single): variant; virtual;
  public
    constructor Create;overload;virtual;
    destructor Destroy; override;
    procedure AddKeyframe(const ATime: TFloatTime; const AValue: variant;
      const ALerpFunc: TLerpFunc = nil);
    procedure Evaluate(const ATime: TFloatTime);
    function Duration: TFloatTime;

    property Mode: TAnimationTrackMode read FMode write FMode;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TAnimationPropertyTrack = class(TAnimationTrack)
  strict private
    FComponent: TPersistent;
    FProperty: string;
    FPropertyInfo: PPropInfo;
  strict protected
    procedure SetValue(const AValue: variant); override;
    function CalcValue(const Value1, Value2: variant; const ALerp: Single): variant; override;
  public
    constructor Create(AComponent: TPersistent; const AProperty: string);overload;
  end;

  TAnimationVector3Track = class abstract(TAnimationTrack)
  strict protected
    function CalcValue(const Value1, Value2: variant; const ALerp: Single): variant; override;
  public
    procedure AddKeyframe(const ATime: TFloatTime; const AValue: TVector3;
      const ALerpFunc: TLerpFunc = nil);
  end;

  TAnimationTrackList = class(
    {$IFDEF FPC}specialize{$ENDIF} TObjectList<TAnimationTrack>)
  end;

  TAnimation = class
  strict private
    FMaxTime: TFloatTime;
    FOnComplete: TNotifyEvent;
    FTrackList: TAnimationTrackList;
    FCurrentTime: TFloatTime;
    FPlaying: boolean;
    FLoop: boolean;
    FSpeed: single;
    procedure SetOnComplete(const AValue: TNotifyEvent);
    procedure TrackListNotify(ASender: TObject;
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationTrack;
        AAction: TCollectionNotification);
    procedure SetPlaying(const Value: boolean);
    procedure SetLoop(const Value: boolean);
    procedure SetSpeed(const Value: single);
    procedure Changed;
    procedure TrackChange(Sender: TObject);
  protected
    procedure Update(const DeltaTime: TFloatTime);

    property OnComplete: TNotifyEvent read FOnComplete write SetOnComplete;
    property Playing: boolean read FPlaying write SetPlaying default False;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTrack(const Track: TAnimationTrack);
    procedure RemoveTrack(const Track: TAnimationTrack);
    procedure ClearTracks;
    procedure Start(const ResetTime: boolean = True);
    procedure Stop(const ResetTime: boolean = True);

    property MaxTime: TFloatTime read FMaxTime;
    property Loop: boolean read FLoop write SetLoop default False;
    property Speed: single read FSpeed write SetSpeed {$IFDEF FPC}default 1{$ENDIF};
  end;

  TAnimationList = {$IFDEF FPC}specialize{$ENDIF} TObjectDictionary<string, TAnimation>;


  TAnimationPlayer = class(TCastleComponent)
  private
    FAnimation: string;
    FCurrentAnimation: TAnimation;
    FAnimationList: TAnimationList;
    FOnAnimationComplete: TNotifyEvent;
    FPlaying: boolean;
    procedure SetOnAnimationComplete(const AValue: TNotifyEvent);
    procedure UpdateAnimation;
    procedure SetAnimation(const AValue: string);
    procedure SetPlaying(const AValue: boolean);

    procedure InternalAnimationComplete(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: string): TPropertySections; override;
    procedure Update(const DeltaTime: TFloatTime);
    procedure AddAnimation(const AName: string; const AAnimation: TAnimation);
    function AnimationExists(const AName: string): boolean;
    procedure RemoveAnimation(const AName: string);
    procedure ClearAnimations;
    procedure Start(const ResetTime: boolean = True);
    procedure Stop(const ResetTime: boolean = True);

    property Animation: string read FAnimation write SetAnimation;
  published

    property Playing: boolean read FPlaying write SetPlaying default False;
    property OnAnimationComplete: TNotifyEvent
      read FOnAnimationComplete write SetOnAnimationComplete;
  end;

  function VariantToVector2(const V: Variant): TVector2;
  function VariantFromVector2(const V: TVector2): Variant;
  function VariantToVector3(const V: Variant): TVector3;
  function VariantFromVector3(const V: TVector3): Variant;
  function VariantToVector4(const V: Variant): TVector4;
  function VariantFromVector4(const V: TVector4): Variant;

implementation

uses Math, Generics.Defaults, Generics.Strings;

function VariantToVector2(const V: Variant): TVector2;
begin
  Result := Vector2(V[0], V[1]);
end;

function VariantFromVector2(const V: TVector2): Variant;
begin
  Result := VarArrayOf([V.X, V.Y]);
end;

function VariantToVector3(const V: Variant): TVector3;
begin
  Result := Vector3(V[0], V[1], V[2]);
end;

function VariantFromVector3(const V: TVector3): Variant;
begin
  Result := VarArrayOf([V.X, V.Y, V.Z]);
end;

function VariantToVector4(const V: Variant): TVector4;
begin
  Result := Vector4(V[0], V[1], V[2], V[3]);
end;

function VariantFromVector4(const V: TVector4): Variant;
begin
  Result := VarArrayOf([V.X, V.Y, V.Z, V.W]);
end;

destructor TAnimationTrack.Destroy;
begin
  FKeyframes.Free;
  inherited Destroy;
end;

procedure TAnimationTrack.AddKeyframe(const ATime: TFloatTime;
  const AValue: variant; const ALerpFunc: TLerpFunc);
var
  Keyframe: TAnimationKeyframe;
begin
  Keyframe.Time := ATime;
  Keyframe.Value := AValue;
  Keyframe.LerpFunc := ALerpFunc;
  FKeyframes.Add(Keyframe);
end;

procedure TAnimationTrack.Evaluate(const ATime: TFloatTime);
var
  AValue: variant;
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
  SetValue(AValue);
end;

function TAnimationTrack.Duration: TFloatTime;
begin
  if FKeyframes.Count < 2 then Exit(0);
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

procedure TAnimation.TrackListNotify(ASender: TObject;
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationTrack;
  AAction: TCollectionNotification);
begin
  Changed;
end;

procedure TAnimation.SetOnComplete(const AValue: TNotifyEvent);
begin
  if FOnComplete = AValue then Exit;
  FOnComplete := AValue;
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
  FTrackList.OnNotify := {$IFDEF FPC}@{$ENDIF}TrackListNotify;
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
  Track.OnChange := {$IFDEF FPC}@{$ENDIF}TrackChange;
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
  if MaxTime <= 0 then Exit;
  //if CastleDesignMode then Exit;

  FCurrentTime := FCurrentTime + DeltaTime * FSpeed;
  if FLoop then
    FCurrentTime := FCurrentTime mod MaxTime
  else if FCurrentTime > MaxTime then
  begin
    Stop(False);
    if Assigned(FOnComplete) then FOnComplete(Self);
  end;
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
  if not FPlaying then FPlaying := True;
end;

procedure TAnimation.Changed;
var
  Track: TAnimationTrack;
begin
  FMaxTime := 0;
  for Track in FTrackList do
  begin
    if Track.Duration > FMaxTime then
      FMaxTime := Track.Duration;
  end;
end;

procedure TAnimation.TrackChange(Sender: TObject);
begin
  Changed;
end;

function TAnimationTrack.Interpolate(const Keyframe1, Keyframe2: TAnimationKeyframe;
  const Time: TFloatTime): variant;
var
  ALerp: single;
begin
  if VarType(Keyframe1.Value) <> VarType(Keyframe2.Value) then
    raise Exception.Create(
      'TAnimationTrack.Interpolate: Interpolation of different value types is not supported.');

  case self.Mode of
    amDiscrete: Result := Keyframe1.Value;
    amContinuous:
    begin
      ALerp := (Time - Keyframe1.Time) / (Keyframe2.Time - Keyframe1.Time);
      if Assigned(Keyframe1.LerpFunc) then ALerp := Keyframe1.LerpFunc(ALerp);
      Result := CalcValue(Keyframe1.Value, Keyframe2.Value, ALerp);
    end;
  end;
end;

procedure TAnimationTrack.SetOnChange(const AValue: TNotifyEvent);
begin
  if FOnChange <> AValue then FOnChange := AValue;
end;

function TAnimationTrack.CalcValue(const Value1, Value2: variant;
  const ALerp: Single): variant;
var
  V1_int, V2_int: int64;
  V1_float, V2_float: extended;
begin
    if VarIsOrdinal(Value1) and (VarIsOrdinal(Value2)) then
    begin
      V1_int := Value1;
      V2_int := Value2;
      Result := Round((1 - ALerp) * V1_int + ALerp * V2_int);
    end
    else if VarIsFloat(Value1) and (VarIsFloat(Value2)) then
    begin
      V1_float := Value1;
      V2_float := Value2;
      Result := (1 - ALerp) * V1_float + ALerp * V2_float;
    end
    else
      raise Exception.CreateFmt(
        'TAnimationTrack.Interpolate: Unsupported variant type [%d][%d]',
        [VarType(Value1), VarType(Value2)]);


end;

function CompareKeyframe({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} Left,
  Right: TAnimationTrack.TAnimationKeyframe): integer;
begin
  Result := CompareValue(Left.Time, Right.Time);
end;

constructor TAnimationTrack.Create;
type
  TInternalKeyframeComparer = {$IFDEF FPC}specialize{$ENDIF} TComparer<TAnimationKeyframe>;
begin
  inherited Create;
  FKeyframes := TAnimationKeyframeList.Create(TInternalKeyframeComparer.Construct(
    {$IFDEF FPC}@{$ENDIF}CompareKeyframe));
  FKeyframes.OnNotify := {$IFDEF FPC}@{$ENDIF}KeyframesNotify;
end;

procedure TAnimationTrack.KeyframesNotify(ASender: TObject;
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationKeyframe;
  AAction: TCollectionNotification);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TAnimationTrack.TAnimationKeyframeList.SearchIndex(
  const AValue: TAnimationKeyframe): SizeInt;
var
  LSearchResult: TBinarySearchResult;
begin
  if {$IFDEF FPC}specialize{$ENDIF} TArrayHelper<TAnimationKeyframe>.BinarySearch(FItems, AValue,
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

procedure TAnimationPropertyTrack.SetValue(const AValue: variant);
begin
  SetPropValue(FComponent, FPropertyInfo, AValue);
end;

function TAnimationPropertyTrack.CalcValue(const Value1, Value2: variant;
  const ALerp: Single): variant;
var
  V1_int, V2_int: int64;
  V1_float, V2_float: extended;
  Tk: TTypeKind;
begin
  Tk := FPropertyInfo^.PropType^.Kind;
  case Tk of
    tkInteger, tkInt64, tkEnumeration,
    tkSet, tkChar, tkWChar:
    begin
      V1_int := Value1;
      V2_int := Value2;
      Result := Round((1 - ALerp) * V1_int + ALerp * V2_int);
    end;
    tkFloat:
    begin
      V1_float := Value1;
      V2_float := Value2;
      Result := (1 - ALerp) * V1_float + ALerp * V2_float;
    end;
    else
      raise Exception.CreateFmt(
        'TAnimationTrack.Interpolate: Unsupported value type[%d], Property:%s.',
        [Tk, FProperty]);
  end;

end;

constructor TAnimationPropertyTrack.Create(AComponent: TPersistent;
  const AProperty: string);
begin
  Create;
  FComponent := AComponent;
  FProperty := AProperty;
  FPropertyInfo := GetPropInfo(FComponent, FProperty);
  if not Assigned(FPropertyInfo) then
    raise Exception.CreateFmt('%s does not exist in %s',
      [FProperty, FComponent.ClassName]);
end;

function TAnimationVector3Track.CalcValue(const Value1, Value2: variant;
  const ALerp: Single): variant;
var
  V1, V2, V3: TVector3;
begin
  V1 := VariantToVector3(Value1);
  V2 := VariantToVector3(Value2);
  V3 := (1 - ALerp) * V1 + ALerp * V2;
  Result := VariantFromVector3(V3);
end;

procedure TAnimationVector3Track.AddKeyframe(const ATime: TFloatTime;
  const AValue: TVector3; const ALerpFunc: TLerpFunc);
begin
  inherited AddKeyframe(ATime, VariantFromVector3(AValue), ALerpFunc);
end;

procedure TAnimation.Stop(const ResetTime: boolean);
begin
  if ResetTime then
  begin
    FCurrentTime := 0;
    Update(0.0);
  end;
  if FPlaying then FPlaying := False;
end;

procedure TAnimationPlayer.UpdateAnimation;
begin
  if not Assigned(FCurrentAnimation) then exit;
  if FPlaying then FCurrentAnimation.Start(True)
  else
    FCurrentAnimation.Stop(True);
end;

procedure TAnimationPlayer.SetOnAnimationComplete(const AValue: TNotifyEvent);
begin
  if FOnAnimationComplete <> AValue then FOnAnimationComplete := AValue;
end;

procedure TAnimationPlayer.SetAnimation(const AValue: string);
begin
  if FAnimation <> AValue then
  begin
    FAnimation := AValue;
    FCurrentAnimation := nil;
    if not FAnimationList.TryGetValue(FAnimation, FCurrentAnimation) then
    begin
      FAnimation := '';
      WritelnWarning('AnimationPlayer', 'Animation "%s" not exists', [AValue]);
    end;
    UpdateAnimation;
  end;

end;

procedure TAnimationPlayer.SetPlaying(const AValue: boolean);
begin
  if FPlaying <> AValue then
  begin
    FPlaying := AValue;
    UpdateAnimation;
  end;
end;

procedure TAnimationPlayer.InternalAnimationComplete(Sender: TObject);
begin
  if Sender = FCurrentAnimation then if Assigned(FOnAnimationComplete) then
      FOnAnimationComplete(Self);
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

function TAnimationPlayer.PropertySections(
  const PropertyName: string): TPropertySections;
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

  if AnimationExists(AName) then
    raise Exception.CreateFmt('AnimationPlayer: Name "%s" already exists', [AName]);

  AAnimation.OnComplete := {$IFDEF FPC}@{$ENDIF}InternalAnimationComplete;
  FAnimationList.Add(AName, AAnimation);
end;

function TAnimationPlayer.AnimationExists(const AName: string): boolean;
begin
  Result := FAnimationList.ContainsKey(AName);
end;

procedure TAnimationPlayer.RemoveAnimation(const AName: string);
begin
  if FAnimation = AName then
    Animation := '';

  FAnimationList.Remove(AName);
end;

procedure TAnimationPlayer.ClearAnimations;
begin
  Animation := '';
  FAnimationList.Clear;
end;

procedure TAnimationPlayer.Start(const ResetTime: boolean);
begin
  if not FPlaying then FPlaying := True;
  if Assigned(FCurrentAnimation) then FCurrentAnimation.Start(ResetTime);
end;

procedure TAnimationPlayer.Stop(const ResetTime: boolean);
begin
  if FPlaying then FPlaying := False;
  if Assigned(FCurrentAnimation) then FCurrentAnimation.Stop(ResetTime);
end;


end.