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
  TAnimationTrackMode = (tmDiscrete, tmContinuous);
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
      {$IFDEF FPC}specialize{$ENDIF} TList<TAnimationKeyframe>)
    protected
      function SearchIndex(const AValue: TAnimationKeyframe): SizeInt;

    end;

  strict private
    FOnChange: TNotifyEvent;
    FKeyframeList: TAnimationKeyframeList;
    FMode: TAnimationTrackMode;
    procedure KeyframesNotify(ASender: TObject;
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationKeyframe; AAction: TCollectionNotification);
    function Interpolate(const Keyframe1, Keyframe2: TAnimationKeyframe;
      const Time: TFloatTime): variant;
    procedure SetOnChange(const AValue: TNotifyEvent);
  private
    { This notification is used by @link(TAnimation), please do not use it. }
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  strict protected
    procedure SetValue(const AValue: variant); virtual; abstract;
    function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
      virtual;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;
    { Add a keyframe. The time is calculated in seconds, with the time when the animation
      starts running as the zero second. LerpFunc represents the equation for modifying the
      original Lerp, if it's nil, it means that Lerp is not modified. Lerp always ranges from 0 to 1. }
    procedure AddKeyframe(const ATime: TFloatTime; const AValue: variant;
      const ALerpFunc: TLerpFunc = nil);
    { Calculate the value corresponding to the time and execute it. }
    procedure Evaluate(const ATime: TFloatTime);
    { The duration of this animation track is determined by the interval between
      the first and last frames. }
    function Duration: TFloatTime;
    { Interpolation mode, there are two types: discrete or continuous.
      If it is continuous, you can define an interpolation calculation equation
      for some keyframes by yourself (see @link(AddKeyframe)). If it is in the discrete mode,
      this equation will be ignored. }
    property Mode: TAnimationTrackMode read FMode write FMode;
    property KeyframeList: TAnimationKeyframeList read FKeyframeList;
  end;

  TAnimationPropertyTrack = class(TAnimationTrack)
  strict private
    FComponent: TPersistent;
    FProperty: string;
    FPropertyInfo: PPropInfo;
  strict protected
    procedure SetValue(const AValue: variant); override;
    function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
      override;
  public
    constructor Create(AComponent: TPersistent; const AProperty: string); overload;
  end;

  TAnimationVector2Track = class abstract(TAnimationTrack)
  strict protected
    function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
      override;
  public
    procedure AddKeyframe(const ATime: TFloatTime; const AValue: TVector2;
      const ALerpFunc: TLerpFunc = nil);
  end;

  TAnimationVector3Track = class abstract(TAnimationTrack)
  strict protected
    function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
      override;
  public
    procedure AddKeyframe(const ATime: TFloatTime; const AValue: TVector3;
      const ALerpFunc: TLerpFunc = nil);
  end;

  TAnimationVector4Track = class abstract(TAnimationTrack)
  strict protected
    function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
      override;
  public
    procedure AddKeyframe(const ATime: TFloatTime; const AValue: TVector4;
      const ALerpFunc: TLerpFunc = nil);
  end;

  TAnimationTrackList = class(
    {$IFDEF FPC}specialize{$ENDIF} TObjectList<TAnimationTrack>)
  end;

  TAnimationPlayStyle = (apsOnce, apsLoop, apsPingPong, apsPingPongOnce);

  TAnimation = class
  strict private
    FMaxTime: TFloatTime;
    FOnComplete: TNotifyEvent;
    FPlayStyle: TAnimationPlayStyle;
    FTrackList: TAnimationTrackList;
    FCurrentTime: TFloatTime;
    FPlaying: boolean;
    FSpeed: single;
    procedure SetOnComplete(const AValue: TNotifyEvent);
    procedure SetPlayStyle(const AValue: TAnimationPlayStyle);
    procedure TrackListNotify(ASender: TObject;
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationTrack; AAction: TCollectionNotification);
    procedure SetPlaying(const Value: boolean);
    procedure SetSpeed(const Value: single);
    function Loop: boolean;
    { Whenever KeyFrameList or TrackList changes, this function will be triggered.
      Then we update the value of FMaxTime.}
    procedure Changed;
    procedure TrackChange(Sender: TObject);
    function GetPingPongEvalTime: TFloatTime;
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
    { The maximum duration among all tracks. }
    property MaxTime: TFloatTime read FMaxTime;
    { apsOnce: The animation plays only once and stops when it finishes.
      apsLoop: The animation plays in a loop, infinitely.
      apsPingPong: The animation plays forward once, then plays backward once, and repeats in this way.
      apsPingPongOnce: The animation plays forward once, then plays backward once, then stops.}
    property PlayStyle: TAnimationPlayStyle
      read FPlayStyle write SetPlayStyle default apsOnce;
    property Speed: single read FSpeed write SetSpeed {$IFDEF FPC}default 1{$ENDIF};
    property TrackList: TAnimationTrackList read FTrackList;
  end;

  TAnimationPlayer = class(TCastleComponent)
  public
  type
    TAnimationList = {$IFDEF FPC}specialize{$ENDIF} TObjectDictionary<string, TAnimation>;
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
    procedure EnsureAnimationNameUnique(const AName: string);
    procedure AddAnimationNoCheck(const AName: string; const AAnimation: TAnimation);

    procedure InternalAnimationComplete(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: string): TPropertySections; override;
    procedure Update(const DeltaTime: TFloatTime);
    procedure AddAnimation(const AName: string; const AAnimation: TAnimation);
    function NewAnimation(const AName: string): TAnimation;
    function AnimationExists(const AName: string): boolean;
    procedure RemoveAnimation(const AName: string);
    procedure ClearAnimations;
    procedure Start(const ResetTime: boolean = True);
    procedure Stop(const ResetTime: boolean = True);

    property Animation: string read FAnimation write SetAnimation;
    property AnimationList: TAnimationList read FAnimationList;
    property CurrentAnimation: TAnimation read FCurrentAnimation;
  published
    property Playing: boolean read FPlaying write SetPlaying default False;
    property OnAnimationComplete: TNotifyEvent
      read FOnAnimationComplete write SetOnAnimationComplete;
  end;

function FloatMod(a, b: TFloatTime): TFloatTime;

function VariantToVector2(const V: variant): TVector2;
function VariantFromVector2(const V: TVector2): variant;
function VariantToVector3(const V: variant): TVector3;
function VariantFromVector3(const V: TVector3): variant;
function VariantToVector4(const V: variant): TVector4;
function VariantFromVector4(const V: TVector4): variant;

implementation

uses Math, Generics.Defaults;

function FloatMod(a, b: TFloatTime): TFloatTime;
begin
  Result := a - b * Floor(a / b);
end;

function VariantToVector2(const V: variant): TVector2;
begin
  Result := Vector2(V[0], V[1]);
end;

function VariantFromVector2(const V: TVector2): variant;
begin
  Result := VarArrayOf([V.X, V.Y]);
end;

function VariantToVector3(const V: variant): TVector3;
begin
  Result := Vector3(V[0], V[1], V[2]);
end;

function VariantFromVector3(const V: TVector3): variant;
begin
  Result := VarArrayOf([V.X, V.Y, V.Z]);
end;

function VariantToVector4(const V: variant): TVector4;
begin
  Result := Vector4(V[0], V[1], V[2], V[3]);
end;

function VariantFromVector4(const V: TVector4): variant;
begin
  Result := VarArrayOf([V.X, V.Y, V.Z, V.W]);
end;

destructor TAnimationTrack.Destroy;
begin
  FKeyframeList.Free;
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
  FKeyframeList.Add(Keyframe);
end;

procedure TAnimationTrack.Evaluate(const ATime: TFloatTime);
var
  AValue: variant;
  Index: SizeInt;
  Keyframe: TAnimationKeyframe;
begin
  if FKeyframeList.Count = 0 then
    Exit;

  if ATime < FKeyframeList.First.Time then
    AValue := FKeyframeList.First.Value
  else
  begin
    Keyframe.Time := ATime;
    Index := FKeyframeList.SearchIndex(Keyframe) - 1;
    if Between(Index, 0, FKeyframeList.Count - 2) then
      AValue := Interpolate(FKeyframeList[Index], FKeyframeList[Index + 1], ATime)
    else
      AValue := FKeyframeList.Last.Value;
  end;
  SetValue(AValue);
end;

function TAnimationTrack.Duration: TFloatTime;
begin
  if FKeyframeList.Count < 2 then Exit(0);
  Result := FKeyframeList.Last.Time - FKeyframeList.First.Time;
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
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationTrack; AAction: TCollectionNotification);
begin
  Changed;
end;

procedure TAnimation.SetOnComplete(const AValue: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnComplete), TMethod(AValue)) then
    FOnComplete := AValue;
end;

procedure TAnimation.SetPlayStyle(const AValue: TAnimationPlayStyle);
begin
  if FPlayStyle <> AValue then
  begin
    FPlayStyle := AValue;
    if FPlayStyle in [apsPingPong, apsPingPongOnce] then
      FCurrentTime := GetPingPongEvalTime;
  end;

end;

procedure TAnimation.SetSpeed(const Value: single);
begin
  if FSpeed <> Value then
  begin
    FSpeed := Value;
  end;
end;

function TAnimation.Loop: boolean;
begin
  Result := FPlayStyle in [apsLoop, apsPingPong];
end;

constructor TAnimation.Create;
begin
  inherited;
  FTrackList := TAnimationTrackList.Create(True);
  FTrackList.OnNotify := {$Ifdef fpc}@{$endif}TrackListNotify;
  FCurrentTime := 0;
  FMaxTime := 0;
  FPlaying := False;
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
  Track.OnChange := {$Ifdef fpc}@{$endif}TrackChange;
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
  EvalTime: TFloatTime;
  bCompleted: boolean;
begin
  if not FPlaying then  Exit;
  if MaxTime <= 0 then Exit;
  //if CastleDesignMode then Exit;

  bCompleted := False;
  FCurrentTime := FCurrentTime + DeltaTime * FSpeed;

  //delphi not support: FCurrentTime := FCurrentTime mod MaxTime
  case FPlayStyle of
    apsLoop:
    begin
      FCurrentTime := FloatMod(FCurrentTime, MaxTime);
      EvalTime := FCurrentTime;
    end;
    apsPingPong:
    begin
      EvalTime := GetPingPongEvalTime;
    end;
    apsPingPongOnce:
    begin
      bCompleted := FCurrentTime >= 2 * MaxTime;
      EvalTime := GetPingPongEvalTime;
    end;
    apsOnce:
    begin
      bCompleted := FCurrentTime >= MaxTime;
      EvalTime := FCurrentTime;
    end;//no others
  end;

  for I := 0 to FTrackList.Count - 1 do
  begin
    Track := TAnimationTrack(FTrackList[I]);
    Track.Evaluate(EvalTime);
  end;

  { Execute finally to ensure the last frame is completed. }
  if bCompleted then
  begin
    Stop(False);
    if Assigned(FOnComplete) then FOnComplete(Self);
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

function TAnimation.GetPingPongEvalTime: TFloatTime;
begin
  FCurrentTime := FloatMod(FCurrentTime, 2 * MaxTime);
  Result := FCurrentTime;
  if Result >= MaxTime then
    Result := 2 * MaxTime - Result;
end;

function TAnimationTrack.Interpolate(const Keyframe1, Keyframe2: TAnimationKeyframe;
  const Time: TFloatTime): variant;
var
  ALerp: single;
begin
  case self.Mode of
    tmDiscrete: Result := Keyframe1.Value;
    tmContinuous:
    begin
      ALerp := (Time - Keyframe1.Time) / (Keyframe2.Time - Keyframe1.Time);
      if Assigned(Keyframe1.LerpFunc) then ALerp := Keyframe1.LerpFunc(ALerp);
      Result := CalcValue(Keyframe1.Value, Keyframe2.Value, ALerp);
    end;
  end;
end;

procedure TAnimationTrack.SetOnChange(const AValue: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnChange), TMethod(AValue)) then
    FOnChange := AValue;
end;

function TAnimationTrack.CalcValue(const Value1, Value2: variant;
  const ALerp: single): variant;
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

function CompareKeyframe({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif}
  Left, Right: TAnimationTrack.TAnimationKeyframe): integer;
begin
  Result := CompareValue(Left.Time, Right.Time);
end;

constructor TAnimationTrack.Create;
type
  TInternalKeyframeComparer =
    {$IFDEF FPC}specialize{$ENDIF}TComparer<TAnimationKeyframe>;
begin
  inherited Create;
  FKeyframeList := TAnimationKeyframeList.Create(TInternalKeyframeComparer.Construct(
    {$Ifdef fpc}@{$endif}CompareKeyframe));
  FKeyframeList.OnNotify :=
    {$Ifdef fpc}@{$endif}KeyframesNotify;
end;

procedure TAnimationTrack.KeyframesNotify(ASender: TObject;
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationKeyframe; AAction: TCollectionNotification);
begin
  FKeyframeList.Sort;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TAnimationTrack.TAnimationKeyframeList.SearchIndex(
  const AValue: TAnimationKeyframe): SizeInt;
var
  {$IFDEF fpc}
  L, H: Integer;
  mid, cmp: Integer;
   {$ELSE}
  Index: integer;
  {$endif}
begin
  {$IFDEF fpc}
  //from delphi
  if Count = 0 then
  begin
    Exit(0);
  end;

  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := FComparer.Compare(FItems[mid], AValue);
    if cmp < 0 then
      L := mid + 1
    else if cmp > 0 then
      H := mid - 1
    else
    begin
      repeat
        Dec(mid);
      until (mid < 0) or (FComparer.Compare(FItems[mid], AValue) <> 0);
      Result := mid + 1;
      Exit;
    end;
  end;
  Result := L;
   {$ELSE}
  BinarySearch(AValue, Index);
  Result := Index;
  {$endif}
end;

procedure TAnimationPropertyTrack.SetValue(const AValue: variant);
begin
  SetPropValue(FComponent, FPropertyInfo, AValue);
end;

function TAnimationPropertyTrack.CalcValue(const Value1, Value2: variant;
  const ALerp: single): variant;
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
        [Ord(Tk), FProperty]);
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

function TAnimationVector2Track.CalcValue(const Value1, Value2: variant;
  const ALerp: single): variant;
var
  V1, V2, V3: TVector2;
begin
  V1 := VariantToVector2(Value1);
  V2 := VariantToVector2(Value2);
  V3 := (1 - ALerp) * V1 + ALerp * V2;
  Result := VariantFromVector2(V3);
end;

procedure TAnimationVector2Track.AddKeyframe(const ATime: TFloatTime;
  const AValue: TVector2; const ALerpFunc: TLerpFunc);
begin
  inherited AddKeyframe(ATime, VariantFromVector2(AValue), ALerpFunc);
end;

function TAnimationVector3Track.CalcValue(const Value1, Value2: variant;
  const ALerp: single): variant;
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

function TAnimationVector4Track.CalcValue(const Value1, Value2: variant;
  const ALerp: single): variant;
var
  V1, V2, V3: TVector4;
begin
  V1 := VariantToVector4(Value1);
  V2 := VariantToVector4(Value2);
  V3 := (1 - ALerp) * V1 + ALerp * V2;
  Result := VariantFromVector4(V3);
end;

procedure TAnimationVector4Track.AddKeyframe(const ATime: TFloatTime;
  const AValue: TVector4; const ALerpFunc: TLerpFunc);
begin
  inherited AddKeyframe(ATime, VariantFromVector4(AValue), ALerpFunc);
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
  if not SameMethods(TMethod(FOnAnimationComplete), TMethod(AValue)) then
    FOnAnimationComplete := AValue;
end;

procedure TAnimationPlayer.SetAnimation(const AValue: string);
begin
  if FAnimation <> AValue then
  begin
    FAnimation := AValue;
    FCurrentAnimation := nil;
    if (FAnimation <> '') and not FAnimationList.TryGetValue(FAnimation,
      FCurrentAnimation) then
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

procedure TAnimationPlayer.EnsureAnimationNameUnique(const AName: string);
begin
  if AName = '' then
    raise Exception.Create('AnimationPlayer: Name must not be empty');

  if AnimationExists(AName) then
    raise Exception.CreateFmt('AnimationPlayer: Name "%s" already exists', [AName]);
end;

procedure TAnimationPlayer.AddAnimationNoCheck(const AName: string;
  const AAnimation: TAnimation);
begin
  AAnimation.OnComplete := {$Ifdef fpc}@{$endif}InternalAnimationComplete;
  FAnimationList.Add(AName, AAnimation);
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
  EnsureAnimationNameUnique(AName);
  if not Assigned(AAnimation) then
    raise Exception.Create('AnimationPlayer: TAnimation is nil');

  AddAnimationNoCheck(AName, AAnimation);
end;

function TAnimationPlayer.NewAnimation(const AName: string): TAnimation;
begin
  EnsureAnimationNameUnique(AName);
  Result := TAnimation.Create;
  AddAnimationNoCheck(AName, Result);
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
