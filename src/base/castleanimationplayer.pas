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
  Generics.Collections, CastleTimeUtils, CastleLog, Variants, CastleVectors;

type
  TAnimationTrackMode = (tmContinuous, tmDiscrete);
  TLerpFunc = function(const ALerp: single): single;

  TLerpFuncType = (lftCustom, lftLiner, lftSin, lftCos, lftOneMinusSin, lftOneMinusCos,
    lftUniformDeceleration);

const
  TLerpFuncArr: array[TLerpFuncType] of TLerpFunc =
    (nil, {$Ifdef fpc}@{$endif}LerpFuncLiner, {$Ifdef fpc}@{$endif}LerpFuncSin,
    {$Ifdef fpc}@{$endif}LerpFuncCos, {$Ifdef fpc}@{$endif}LerpFuncOneMinusSin,
    {$Ifdef fpc}@{$endif}LerpFuncOneMinusCos,
    {$Ifdef fpc}@{$endif}LerpFuncUniformDeceleration);

type
  { Inherit from TPersistent for RegisterClass and de/serilization referenced component. }
  TAnimationTrack = class abstract(TPersistent)
  public
  type
    TKeyFrameChangeType = (kfcTime, kfcValue, kfcLerpFunc);
    TKeyFrameChangeEvent = procedure(ASender: TObject;
      const AChangeType: TKeyFrameChangeType) of object;

    TAnimationKeyframe = class
    private
      FLerpFunc: TLerpFunc;
      FOnChange: TKeyFrameChangeEvent;
      FTime: TFloatTime;
      FValue: variant;
      procedure SetLerpFunc(const AValue: TLerpFunc);
      procedure SetOnChange(const AValue: TKeyFrameChangeEvent);
      procedure SetTime(const AValue: TFloatTime);
      procedure SetValue(const AValue: variant);
      procedure Changed(const AChangeType: TKeyFrameChangeType);

      //Used by KeyFrameList, donot set it.
      property OnChange: TKeyFrameChangeEvent read FOnChange write SetOnChange;
    public
      // only for serialize
      LerpFuncType: TLerpFuncType;
      property Value: variant read FValue write SetValue;
      property LerpFunc: TLerpFunc read FLerpFunc write SetLerpFunc;
      property Time: TFloatTime read FTime write SetTime;
    end;

    TAnimationKeyframeList = class(
      {$IFDEF FPC}specialize{$ENDIF} TObjectList<TAnimationKeyframe>)
    strict private
      function SearchIndex(const ATime: TFloatTime): SizeInt;
    public
      function TimeToKeyFrame(const ATime: TFloatTime): SizeInt;
    end;

  strict private
    FOnChange: TNotifyEvent;
    FKeyframeList: TAnimationKeyframeList;
    FMode: TAnimationTrackMode;
    procedure KeyFramInTrackChange(ASender: TObject;
      const AChangeType: TKeyFrameChangeType);
    procedure KeyframesNotify(ASender: TObject;
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationKeyframe; AAction: TCollectionNotification);
    function Interpolate(const Keyframe1, Keyframe2: TAnimationKeyframe;
      const Time: TFloatTime): variant;
    procedure SetOnChange(const AValue: TNotifyEvent);
  private
    procedure SetFriendlyObjectName(const AValue: string);
    { This notification is used by @link(TAnimation), please do not use it. }
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  strict protected
    FFriendlyObjectName: string;
    function GetFriendlyObjectName: string; virtual;
    procedure SetValue(const AValue: variant); virtual; abstract;
    function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
      virtual;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;
    function ObjectName: string; virtual;
    function PropName: string; virtual;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess;
      const APath: string; const bReading: boolean; const APlayer: TComponent); virtual;
    { Add a keyframe. The time is calculated in seconds, with the time when the animation
      starts running as the zero second. LerpFunc represents the equation for modifying the
      original Lerp, if it's nil, it means that Lerp is not modified. Lerp always ranges from 0 to 1. }
    function AddKeyframe(const ATime: TFloatTime; const AValue: variant;
      const ALerpFunc: TLerpFunc = nil): TAnimationKeyframe; overload;
    function AddKeyframe(const AValue: TAnimationKeyframe): TAnimationKeyframe;
      overload;
    { Add the value of the current object as a keyframe and return a value indicating success or failure. }
    function AddKeyframeAtTime(const ATime: TFloatTime;
      const ALerpFunc: TLerpFunc = nil): boolean; virtual;
    { Calculate the value corresponding to the time and execute it. }
    procedure Evaluate(const ATime: TFloatTime);
    { The duration of this animation track is determined by the sorted last frame. }
    function Duration: TFloatTime;
    { The names of some sub-controls are empty, so you can manually set the names
      with hierarchical structures here. For example, Box1.XX. }
    property FriendlyObjectName: string read GetFriendlyObjectName
      write SetFriendlyObjectName;
    { Interpolation mode, there are two types: discrete or continuous.
      If it is continuous, you can define an interpolation calculation equation
      for some keyframes by yourself (see @link(AddKeyframe)). If it is in the discrete mode,
      this equation will be ignored. }
    property Mode: TAnimationTrackMode read FMode write FMode;
    property KeyframeList: TAnimationKeyframeList read FKeyframeList;
  end;

  TAnimationTrackClass = class of TAnimationTrack;

  TAnimationVector2Track = class abstract(TAnimationTrack)
  strict protected
    function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
      override;
  public
    function AddKeyframe(const ATime: TFloatTime; const AValue: TVector2;
      const ALerpFunc: TLerpFunc = nil): TAnimationTrack.TAnimationKeyframe;
  end;

  TAnimationVector3Track = class abstract(TAnimationTrack)
  strict protected
    function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
      override;
  public
    function AddKeyframe(const ATime: TFloatTime; const AValue: TVector3;
      const ALerpFunc: TLerpFunc = nil): TAnimationTrack.TAnimationKeyframe;
  end;

  TAnimationVector4Track = class abstract(TAnimationTrack)
  strict protected
    function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
      override;
  public
    function AddKeyframe(const ATime: TFloatTime; const AValue: TVector4;
      const ALerpFunc: TLerpFunc = nil): TAnimationTrack.TAnimationKeyframe;
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
    procedure SetActualCurrentTime(const AValue: TFloatTime);
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
    procedure UpdateByCurrentTime(out bCompleted: boolean);
    procedure UpdateByTime(const ATime: TFloatTime);

    property OnComplete: TNotifyEvent read FOnComplete write SetOnComplete;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTrack(const Track: TAnimationTrack);
    procedure RemoveTrack(const Track: TAnimationTrack);
    procedure ClearTracks;
    procedure Start(const ResetTime: boolean = True);
    procedure Stop(const ResetTime: boolean = True);
    procedure ForceUpdate; overload;
    procedure ForceUpdate(const ATime: TFloatTime); overload;
    function IsEmpty: boolean;

    { FCurrentTime in PingPong mode needs to be corrected. }
    function GetActualCurrentTime: TFloatTime;
    property ActualCurrentTime: TFloatTime read GetActualCurrentTime
      write SetActualCurrentTime;
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
    property Playing: boolean read FPlaying write SetPlaying default False;
  end;

  TAnimationPlayer = class(TCastleComponent)
  public
  type
    TAnimationList = {$IFDEF FPC}specialize{$ENDIF} TObjectDictionary<string, TAnimation>;
  strict private
    FAnimation: string;
    FCurrentAnimation: TAnimation;
    FAnimationList: TAnimationList;
    FOnAnimationComplete: TNotifyEvent;
    FOnCurrentAnimationChanged: TNotifyEvent;
    FPlaying: boolean;
    procedure SetOnAnimationComplete(const AValue: TNotifyEvent);
    procedure SetOnCurrentAnimationChanged(const AValue: TNotifyEvent);
    procedure UpdateAnimation;
    procedure SetAnimation(const AValue: string);
    procedure SetPlaying(const AValue: boolean);
    procedure EnsureAnimationNameUnique(const AName: string);
    procedure AddAnimationNoCheck(const AName: string; const AAnimation: TAnimation);
    procedure InternalAnimationComplete(Sender: TObject);
  protected
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

    procedure CustomSerialization(const SerializationProcess: TSerializationProcess);
      override;

    { Not published, otherwise it will deserialize earlier than @link(CustomSerialization).}
    property Animation: string read FAnimation write SetAnimation;
    property AnimationList: TAnimationList read FAnimationList;
    property CurrentAnimation: TAnimation read FCurrentAnimation;
  published
    property Playing: boolean read FPlaying write SetPlaying default False;
    property OnAnimationComplete: TNotifyEvent
      read FOnAnimationComplete write SetOnAnimationComplete;
    property OnCurrentAnimationChanged: TNotifyEvent
      read FOnCurrentAnimationChanged write SetOnCurrentAnimationChanged;
  end;

function FloatMod(a, b: TFloatTime): TFloatTime;

function VariantToVector2(const V: variant): TVector2;
function VariantFromVector2(const V: TVector2): variant;
function VariantToVector3(const V: variant): TVector3;
function VariantFromVector3(const V: TVector3): variant;
function VariantToVector4(const V: variant): TVector4;
function VariantFromVector4(const V: TVector4): variant;

function VariantLen(const V: variant): integer;
function VariantToString(const V: variant): string;
function VariantFromString(const S: string): variant;


function KeyProp(const SObject, SPropName: string): string;
function KeyItem(const SObject: string; const AIndex: integer): string;
function KeyCount(const SObject: string): string;

procedure ComponentSerialization(const SerializationProcess: TSerializationProcess;
  const AObject: TObject; const APropName, AComponentName, APath: string;
  const bReading: boolean);

implementation

uses Math, Generics.Defaults, TypInfo;

function FloatMod(a, b: TFloatTime): TFloatTime;
begin
  Result := a - b * Floor(a / b);
end;

function VariantLen(const V: variant): integer;
begin
  if VarIsNull(v) then Exit(0);
  if VarIsArray(V) then
    Result := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1
  else
    Result := 1;
end;

function VariantToVector2(const V: variant): TVector2;
begin
  if VariantLen(v) <> 2 then
    raise Exception.CreateFmt('variant len not match. expected:%d get:%d',
      [2, VariantLen(v)]);
  Result := Vector2(V[0], V[1]);
end;

function VariantFromVector2(const V: TVector2): variant;
begin
  Result := VarArrayOf([V.X, V.Y]);
end;

function VariantToVector3(const V: variant): TVector3;
begin
  if VariantLen(v) <> 3 then
    raise Exception.CreateFmt('variant len not match. expected:%d get:%d',
      [3, VariantLen(v)]);
  Result := Vector3(V[0], V[1], V[2]);
end;

function VariantFromVector3(const V: TVector3): variant;
begin
  Result := VarArrayOf([V.X, V.Y, V.Z]);
end;

function VariantToVector4(const V: variant): TVector4;
begin
  if VariantLen(v) <> 4 then
    raise Exception.CreateFmt('variant len not match. expected:%d get:%d',
      [4, VariantLen(v)]);
  Result := Vector4(V[0], V[1], V[2], V[3]);
end;

function VariantFromVector4(const V: TVector4): variant;
begin
  Result := VarArrayOf([V.X, V.Y, V.Z, V.W]);
end;

function VariantToString(const V: variant): string;
var
  I: integer;
  Len: integer;
  ArrPtr: Pointer;
begin
  if VarIsNull(v) then Exit('');
  if VarIsArray(V) then
  begin
    Len := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
    case Len of
      2: Result := Format('(%f,%f)', [V[0], V[1]]);
      3: Result := Format('(%f,%f,%f)', [V[0], V[1], V[2]]);
      4: Result := Format('(%f,%f,%f,%f)', [V[0], V[1], V[2], V[3]]);
      else
        Result := '';
    end;
  end
  else
    Result := VarToStr(V);
end;

function VariantFromString(const S: string): variant;
var
  Tokens: {$Ifdef fpc}specialize{$endif}TArray<string>;
  Len: integer;
begin
  Tokens := S.Trim(['(', ')']).Split([',']);
  Len := Length(Tokens);
  case Len of
    2: Result := VarArrayOf([StrToFloat(Tokens[0]), StrToFloat(Tokens[1])]);
    3: Result := VarArrayOf([StrToFloat(Tokens[0]), StrToFloat(Tokens[1]),
        StrToFloat(Tokens[2])]);
    4: Result := VarArrayOf([StrToFloat(Tokens[0]), StrToFloat(Tokens[1]),
        StrToFloat(Tokens[2]), StrToFloat(Tokens[3])]);
    else
      Result := S;
  end;
end;


function KeyProp(const SObject, SPropName: string): string;
begin
  Result := SObject + '-' + SPropName;
end;

function KeyItem(const SObject: string; const AIndex: integer): string;
begin
  Result := SObject + '_' + AIndex.ToString;
end;

function KeyCount(const SObject: string): string;
begin
  Result := SObject + '-_Count';
end;

procedure ComponentSerialization(const SerializationProcess: TSerializationProcess;
  const AObject: TObject; const APropName, AComponentName, APath: string;
  const bReading: boolean);
var
  s: string;
begin
  s := AComponentName;
  SerializationProcess.ReadWriteString(KeyProp(APath, APropName), s, s <> '');
  if bReading and (s <> '') then
    SerializationProcess.RequireComponent(AObject, GetPropInfo(AObject, APropName), s);
end;

destructor TAnimationTrack.Destroy;
begin
  FKeyframeList.Free;
  inherited Destroy;
end;

function TAnimationTrack.ObjectName: string;
begin
  Result := '';
end;

function TAnimationTrack.PropName: string;
begin
  Result := '';
end;

procedure TAnimationTrack.CustomSerialization(
  const SerializationProcess: TSerializationProcess; const APath: string;
  const bReading: boolean; const APlayer: TComponent);
var
  Aint, I, KeyFrameCount: integer;
  FramePath, s: string;
  Frame: TAnimationKeyframe;
  Afloat: single;
const
  SFrame = 'Frame';
begin
  { Track Properties. }
   { property Mode: TAnimationTrackMode read FMode write FMode;
    property KeyframeList: TAnimationKeyframeList read FKeyframeList;     }
  Aint := Ord(FMode);
  SerializationProcess.ReadWriteInteger(
    KeyProp(APath, 'Mode'),
    Aint, Aint <> 0);
  Mode := TAnimationTrackMode(Aint);
  { KeyFrameList }
  KeyFrameCount := KeyframeList.Count;
  SerializationProcess.ReadWriteInteger(KeyCount(KeyProp(APath, SFrame)),
    KeyFrameCount, KeyFrameCount > 0);
  if KeyFrameCount = 0 then Exit;

  for i := 0 to KeyFrameCount - 1 do
  begin
    if bReading then
    begin
      Frame := TAnimationKeyframe.Create;
      AddKeyframe(Frame);
    end
    else
    begin
      Frame := KeyframeList[I];
    end;
    { KeyFrame propery }
    { LerpFuncType: TLerpFuncType;
      property Value: variant read FValue write SetValue;
      property LerpFunc: TLerpFunc read FLerpFunc write SetLerpFunc;
      property Time: TFloatTime read FTime write SetTime;    }
    FramePath := KeyItem(KeyProp(APath, SFrame), I);

    Aint := Ord(Frame.LerpFuncType);
    SerializationProcess.ReadWriteInteger(
      KeyProp(FramePath, 'LerpFuncType'),
      Aint, Aint <> 0);
    Frame.LerpFuncType := TLerpFuncType(Aint);
    Frame.LerpFunc := TLerpFuncArr[Frame.LerpFuncType];

    Afloat := Frame.Time;
    SerializationProcess.ReadWriteSingle(
      KeyProp(FramePath, 'Time'),
      Afloat, True);
    Frame.Time := Afloat;

    s := VariantToString(Frame.Value);
    SerializationProcess.ReadWriteString(
      KeyProp(FramePath, 'Value'),
      s, s <> '');
    Frame.Value := VariantFromString(s);
  end;

end;

function TAnimationTrack.AddKeyframe(const ATime: TFloatTime;
  const AValue: variant; const ALerpFunc: TLerpFunc): TAnimationKeyframe;
begin
  Result := TAnimationKeyframe.Create;
  Result.Time := ATime;
  Result.Value := AValue;
  Result.LerpFunc := ALerpFunc;
  AddKeyframe(Result);
end;

function TAnimationTrack.AddKeyframe(
  const AValue: TAnimationKeyframe): TAnimationKeyframe;
begin
  AValue.OnChange := {$Ifdef fpc}@{$endif}KeyFramInTrackChange;
  FKeyframeList.Add(AValue);
  Result := AValue;
end;

function TAnimationTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  Result := False;
end;

procedure TAnimationTrack.Evaluate(const ATime: TFloatTime);
var
  AValue: variant;
  Index: SizeInt;
begin
  if FKeyframeList.Count = 0 then
    Exit;

  if ATime < FKeyframeList.First.Time then
    AValue := FKeyframeList.First.Value
  else
  begin
    Index := FKeyframeList.TimeToKeyFrame(ATime);
    if Between(Index, 0, FKeyframeList.Count - 2) then
      AValue := Interpolate(FKeyframeList[Index], FKeyframeList[Index + 1], ATime)
    else
      { If the time is before the first frame or after the last frame, it's considered as the last static frame.}
      AValue := FKeyframeList.Last.Value;
  end;
  SetValue(AValue);
end;

function TAnimationTrack.Duration: TFloatTime;
begin
  if FKeyframeList.Count = 0 then Exit(0);
  Result := FKeyframeList.Last.Time;
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

procedure TAnimation.SetActualCurrentTime(const AValue: TFloatTime);
begin
  if ActualCurrentTime <> AValue then
  begin
    if AValue >= 0 then
      FCurrentTime := AValue
    else
      FCurrentTime := 0;
  end;
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
  bCompleted: boolean;
begin
  if not FPlaying then  Exit;
  if IsEmpty then
  begin
    bCompleted := not Loop;
  end
  else
  begin
    FCurrentTime := FCurrentTime + DeltaTime * FSpeed;
    UpdateByCurrentTime(bCompleted);
  end;
  { Execute finally to ensure the last frame is completed. }
  if bCompleted then
  begin
    Stop(False);
    if Assigned(FOnComplete) then FOnComplete(Self);
  end;
end;

procedure TAnimation.UpdateByCurrentTime(out bCompleted: boolean);
var
  EvalTime: TFloatTime;
  I: integer;
  Track: TAnimationTrack;
begin
  EvalTime := 0;
  bCompleted := False;
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
    end;
  end;

  for I := 0 to FTrackList.Count - 1 do
  begin
    Track := TAnimationTrack(FTrackList[I]);
    Track.Evaluate(EvalTime);
  end;
end;

procedure TAnimation.UpdateByTime(const ATime: TFloatTime);
var
  EvalTime: TFloatTime;
  I: integer;
  Track: TAnimationTrack;
begin
  EvalTime := 0;
  //delphi not support: FCurrentTime := FCurrentTime mod MaxTime
  case FPlayStyle of
    apsLoop:
    begin
      EvalTime := FloatMod(ATime, MaxTime);
    end;
    apsPingPong, apsPingPongOnce:
    begin
      EvalTime := FloatMod(ATime, 2 * MaxTime);
      if EvalTime >= MaxTime then
        EvalTime := 2 * MaxTime - EvalTime;
    end;
    apsOnce:
    begin
      EvalTime := ATime;
    end;
  end;

  for I := 0 to FTrackList.Count - 1 do
  begin
    Track := TAnimationTrack(FTrackList[I]);
    Track.Evaluate(EvalTime);
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

function TAnimationTrack.GetFriendlyObjectName: string;
begin
  Result := FFriendlyObjectName;
  if Result = '' then Result := ObjectName;
end;

procedure TAnimationTrack.SetFriendlyObjectName(const AValue: string);
begin
  if FFriendlyObjectName <> AValue then FFriendlyObjectName := AValue;
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
  FMode := TAnimationTrackMode.tmContinuous;
  FKeyframeList := TAnimationKeyframeList.Create(TInternalKeyframeComparer.Construct(
    {$Ifdef fpc}@{$endif}CompareKeyframe), True);
  FKeyframeList.OnNotify :=
    {$Ifdef fpc}@{$endif}KeyframesNotify;
end;

procedure TAnimationTrack.KeyFramInTrackChange(ASender: TObject;
  const AChangeType: TKeyFrameChangeType);
begin
  if AChangeType = TKeyFrameChangeType.kfcTime then
  begin
    FKeyframeList.Sort;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TAnimationTrack.KeyframesNotify(ASender: TObject;
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationKeyframe; AAction: TCollectionNotification);
begin
  FKeyframeList.Sort;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TAnimationTrack.TAnimationKeyframe.SetTime(const AValue: TFloatTime);
begin
  if FTime = AValue then Exit;
  FTime := AValue;
  Changed(kfcTime);
end;

procedure TAnimationTrack.TAnimationKeyframe.SetValue(const AValue: variant);
begin
  //variant not support: if FValue = AValue then Exit;
  FValue := AValue;
  Changed(kfcValue);
end;

procedure TAnimationTrack.TAnimationKeyframe.Changed(
  const AChangeType: TKeyFrameChangeType);
begin
  if Assigned(FOnChange) then FOnChange(Self, AChangeType);
end;

procedure TAnimationTrack.TAnimationKeyframe.SetOnChange(
  const AValue: TKeyFrameChangeEvent);
begin
  if not SameMethods(TMethod(FOnChange), TMethod(AValue)) then
    FOnChange := AValue;
end;

procedure TAnimationTrack.TAnimationKeyframe.SetLerpFunc(const AValue: TLerpFunc);
begin
  //delphi fail : if FLerpFunc = AValue then Exit;
  FLerpFunc := AValue;
  Changed(kfcLerpFunc);
end;

function TAnimationTrack.TAnimationKeyframeList.SearchIndex(
  const ATime: TFloatTime): SizeInt;

  function Compare(const A, B: TFloatTime): integer;
  begin
    Result := Sign(A - B);
  end;

var
  L, H: integer;
  mid, cmp: integer;
begin
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
    cmp := Compare(Items[mid].Time, ATime);
    if cmp < 0 then
      L := mid + 1
    else if cmp > 0 then
      H := mid - 1
    else
    begin
      repeat
        Dec(mid);
      until (mid < 0) or (Compare(Items[mid].Time, ATime) <> 0);
      //Founded, not "Result := mid + 1;", increase by 1
      Result := mid + 2;
      Exit;
    end;
  end;
  Result := L;
end;

function TAnimationTrack.TAnimationKeyframeList.TimeToKeyFrame(
  const ATime: TFloatTime): SizeInt;
begin
  Result := SearchIndex(ATime) - 1;
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

function TAnimationVector2Track.AddKeyframe(const ATime: TFloatTime;
  const AValue: TVector2; const ALerpFunc: TLerpFunc): TAnimationTrack.TAnimationKeyframe;
begin
  Result := inherited AddKeyframe(ATime, VariantFromVector2(AValue), ALerpFunc);
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

function TAnimationVector3Track.AddKeyframe(const ATime: TFloatTime;
  const AValue: TVector3; const ALerpFunc: TLerpFunc): TAnimationTrack.TAnimationKeyframe;
begin
  Result := inherited AddKeyframe(ATime, VariantFromVector3(AValue), ALerpFunc);
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

function TAnimationVector4Track.AddKeyframe(const ATime: TFloatTime;
  const AValue: TVector4; const ALerpFunc: TLerpFunc): TAnimationTrack.TAnimationKeyframe;
begin
  Result := inherited AddKeyframe(ATime, VariantFromVector4(AValue), ALerpFunc);
end;

procedure TAnimation.Stop(const ResetTime: boolean);
begin
  if ResetTime then
  begin
    FCurrentTime := 0;
    ForceUpdate;
  end;
  if FPlaying then FPlaying := False;
end;

procedure TAnimation.ForceUpdate;
var
  b: boolean;
begin
  UpdateByCurrentTime(b);
end;

procedure TAnimation.ForceUpdate(const ATime: TFloatTime);
begin
  UpdateByTime(ATime);
end;

function TAnimation.IsEmpty: boolean;
begin
  Result := (FTrackList.Count = 0) or (MaxTime <= 0);
end;

function TAnimation.GetActualCurrentTime: TFloatTime;
begin
  if FPlayStyle in [apsPingPong, apsPingPongOnce] then
    Result := GetPingPongEvalTime
  else
    Result := FCurrentTime;
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

procedure TAnimationPlayer.SetOnCurrentAnimationChanged(const AValue: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnCurrentAnimationChanged), TMethod(AValue)) then
    FOnCurrentAnimationChanged := AValue;
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

    if Assigned(FOnCurrentAnimationChanged) then FOnCurrentAnimationChanged(Self);
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

procedure TAnimationPlayer.CustomSerialization(
  const SerializationProcess: TSerializationProcess);

  function CreateTrackByClassName(const AClassName: string): TAnimationTrack;
  var
    AClass: TPersistentClass;
  begin
    AClass := GetClass(AClassName);
    if Assigned(AClass) then Result := TAnimationTrackClass(AClass).Create
    else
      Result := nil;
  end;

const
  SAni = 'Animation';
  STrack = 'Track';
var
  AniCount, TrackCount, I, J, Aint: integer;
  AFloat: single;
  Ani: TAnimation;
  Track: TAnimationTrack;
  s, AniKeyPath, TrackKeyPath: string;
  bReading: boolean;
  AniKeys:{$Ifdef fpc}specialize{$endif}TArray<string>;
begin
  inherited CustomSerialization(SerializationProcess);
  bReading := IsLoading;

  AniCount := AnimationList.Keys.Count;
  SerializationProcess.ReadWriteInteger(KeyCount(SAni), AniCount, AniCount > 0);
  if AniCount = 0 then Exit;

  if not bReading then
  begin
    AniKeys := AnimationList.Keys.ToArray;
    if length(AniKeys) = 0 then Exit;
  end;
  for  I := 0 to AniCount - 1 do
  begin
    AniKeyPath := KeyItem(SAni, I);
    { Get Animation. }
    if bReading then s := ''
    else
      s := AniKeys[i];
    SerializationProcess.ReadWriteString(AniKeyPath, s, s <> '');

    Ani := nil;
    if bReading then
    begin
      if not AnimationExists(S) then Ani := NewAnimation(S)
      else
      begin
        WritelnWarning(
          'CustomSerialization:animation "%s" already exist', [S]);
        Continue;
      end;
    end
    else
    if not AnimationList.TryGetValue(S, Ani) then
    begin
      WritelnWarning(
        'CustomSerialization:animation "%s" not exist', [S]);
      Continue;
    end;

    { Animation Properties. }
    Aint := Ord(Ani.PlayStyle);
    SerializationProcess.ReadWriteInteger(KeyProp(AniKeyPath, 'PlayStyle'),
      Aint, Aint <> 0);
    if bReading then  Ani.PlayStyle := TAnimationPlayStyle(Aint);

    AFloat := Ani.Speed;
    SerializationProcess.ReadWriteSingle(KeyProp(AniKeyPath, 'Speed'),
      AFloat, not SameValue(AFloat, 1));
    if bReading then  Ani.Speed := AFloat;
    { TrackList }
    TrackCount := Ani.TrackList.Count;
    SerializationProcess.ReadWriteInteger(KeyCount(KeyProp(AniKeyPath, STrack)),
      TrackCount, TrackCount > 0);
    if TrackCount = 0 then Continue;

    for  J := 0 to TrackCount - 1 do
    begin
      TrackKeyPath := KeyItem(KeyProp(KeyItem(SAni, I), STrack), J);
      { Get Track. }
      if bReading then  s := ''
      else
        s := Ani.TrackList[J].ClassName;
      SerializationProcess.ReadWriteString(
        KeyProp(TrackKeyPath, 'TrackClass'),
        s, s <> '');
      if s = '' then Continue;
      if bReading then
      begin
        Track := CreateTrackByClassName(s);
        Ani.AddTrack(Track);
      end
      else
      begin
        Track := ani.TrackList[J];
      end;
      if not Assigned(Track) then
      begin
        WritelnWarning('CustomSerialization:Track is nil. Track id: %d TrackClass: "%s"',
          [J, s]);
        Continue;
      end;
      Track.CustomSerialization(SerializationProcess, TrackKeyPath, bReading, Self);
    end;
  end;

  s := Animation;
  SerializationProcess.ReadWriteString('CurrentAnimation', s, s <> '');
  if bReading then Animation := s;
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
  if AnimationExists(AName) then
  begin
    if Animation = AName then
      Animation := '';
    FAnimationList.Remove(AName);
    //if FAnimationList.ContainsKey(AName) then
    //  WritelnWarning('RemoveAnimation: failed to remove animation "%s"', [AName]);
  end
  else
    WritelnWarning('RemoveAnimation: animation "%s" not exist.', [AName]);
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
