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
  Generics.Collections, CastleTimeUtils, CastleLog, Variants, CastleVectors, TypInfo;

type
  TAnimationTrackMode = (tmContinuous, tmDiscrete);
  TLerpFunc = function(const ALerp: single): single;

  TLerpFuncType = (lftCustom, lftLiner, lftSin, lftElastic, lftBack,
    lftOneMinusCos, lftUniformDeceleration, lftQuad,
    lftCubic, lftLowWave, lftMiddleWave,
    lftHighWave, lftBounce, lftCircle);

type
  TKeyFrameChangeType = (kfcTime, kfcValue, kfcLerpFunc);
  TKeyFrameChangeEvent = procedure(ASender: TObject;
    const AChangeType: TKeyFrameChangeType) of object;

  TAnimationTrack = class;

  TAnimationKeyframe = class abstract
  strict private
    FTime: TFloatTime;
    FLerpFunc: TLerpFunc;
    FLerpFuncType: TLerpFuncType;
    FOnChange: TKeyFrameChangeEvent;
    procedure SetLerpFunc(const AValue: TLerpFunc);
    procedure SetLerpFuncType(const AValue: TLerpFuncType);
    procedure SetTime(const AValue: TFloatTime);
    procedure SetOnChange(const AValue: TKeyFrameChangeEvent);

  private
    //Used by KeyFrameList, donot set it.
    property OnChange: TKeyFrameChangeEvent read FOnChange write SetOnChange;
  protected
    procedure Changed(const AChangeType: TKeyFrameChangeType);
    function GetOwnerTrack: TAnimationTrack; virtual; abstract;
  public
    function ValueToString: string; virtual; abstract;
    procedure ValueFromString(const s: string); virtual; abstract;

    property LerpFuncType: TLerpFuncType read FLerpFuncType write SetLerpFuncType;
    property LerpFunc: TLerpFunc read FLerpFunc write SetLerpFunc;
    property Time: TFloatTime read FTime write SetTime;
    property OwnerTrack: TAnimationTrack read GetOwnerTrack;
  end;

  {$Ifdef fpc}generic{$endif}

  TAnimationKeyframeGeneric<T> = class(TAnimationKeyframe)
  strict private
    FValue: T;
    procedure SetValue(const AValue: T);
  protected
    function SameKeyFrameValue(const Value1, Value2: T): boolean; virtual;
  public
    property Value: T read FValue write SetValue;
  end;

  TAnimationKeyframeList = class(
      {$IFDEF FPC}specialize{$ENDIF} TObjectList<TAnimationKeyframe>)
  strict private
    function SearchIndex(const ATime: TFloatTime): SizeInt;
  public
    function TimeToKeyFrame(const ATime: TFloatTime): SizeInt;
  end;

  TAnimation = class;

  { Inherit from TComponent for RegisterClass, de/serilization referenced component, freenotify. }
  TAnimationTrack = class abstract(TComponent)
  strict private
    FOnChange: TNotifyEvent;

    procedure KeyFramInTrackChange(ASender: TObject;
      const AChangeType: TKeyFrameChangeType);
    procedure KeyframesNotify(ASender: TObject;
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationKeyframe; AAction: TCollectionNotification);

    procedure SetOnChange(const AValue: TNotifyEvent);
  private
    { This notification is used by @link(TAnimation), please do not use it. }
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  protected
    FComponent: TComponent;
    FAnimation: TAnimation;
    FKeyframeList: TAnimationKeyframeList;
    { For mode @link(tmDiscrete), no need to execute every frame, so check the last executed frame. }
    FLastExecutedKeyFrame: TAnimationKeyframe;
    FMode: TAnimationTrackMode;
    FFriendlyObjectName: string;
    function GetFriendlyObjectName: string; virtual;
    procedure SetComponent(const AValue: TComponent); virtual;
    procedure SetFriendlyObjectName(const AValue: string); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create; overload; virtual;
    destructor Destroy; override;
    function ObjectName: string; virtual;
    function PropName: string; virtual;
    function ValidValueString(const s: string): boolean; virtual; abstract;

    procedure CustomSerialization(const SerializationProcess: TSerializationProcess;
      const APath: string; const bReading: boolean; const APlayer: TComponent); virtual;
    { Add a keyframe. The time is calculated in seconds, with the time when the animation
      starts running as the zero second. LerpFunc represents the equation for modifying the
      original Lerp, if it's nil, it means that Lerp is not modified. Lerp always ranges from 0 to 1. }
    function NewKeyFrame: TAnimationKeyframe; virtual; abstract;
    function AddKeyframe(const AValue: TAnimationKeyframe): TAnimationKeyframe;
    function RemoveKeyFrame(const AValue: TAnimationKeyframe): SizeInt; overload;
    procedure RemoveKeyFrame(AIndex: SizeInt); overload;
    { Add the value of the current object as a keyframe and return a value indicating success or failure. }
    function AddKeyframeAtTime(const ATime: TFloatTime;
      const ALerpFunc: TLerpFunc = nil): boolean; virtual;
    { Calculate the value corresponding to the time and execute it. }
    procedure Evaluate(const ATime: TFloatTime); virtual; abstract;
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
  published
    property Component: TComponent read FComponent write SetComponent;
  end;

  TAnimationTrackClass = class of TAnimationTrack;

  {$Ifdef fpc}generic{$endif}

  TAnimationTrackGeneric<T> = class abstract(TAnimationTrack)
  private
    function Interpolate(const Keyframe1, Keyframe2: TAnimationKeyframe;
      const Time: TFloatTime): T;
  protected
  type
    TAnimationKeyframeInternal = class
      ({$IFDEF FPC}specialize{$ENDIF}TAnimationKeyframeGeneric<T>)
    private
      FOwner: TAnimationTrackGeneric{$IFNDEF FPC}<T>{$ENDIF} ;
    protected
      function GetOwnerTrack: TAnimationTrack; override;
      function SameKeyFrameValue(const Value1, Value2: T): boolean; override;
    public
      function ValueToString: string; override;
      procedure ValueFromString(const s: string); override;
    end;

    procedure SetValue(const AValue: T); virtual; abstract;
    function KeyFrameValueToString(const AValue: T): string; virtual; abstract;
    function KeyFrameValueFromString(const s: string): T; virtual; abstract;
    function SameKeyFrameValue(const Value1, Value2: T): boolean; virtual;
  public
    function NewKeyFrame: TAnimationKeyframe; override;
    function AddKeyframe(const ATime: TFloatTime; const AValue: T;
      const ALerpFunc: TLerpFunc = nil): TAnimationKeyframeInternal;
    procedure Evaluate(const ATime: TFloatTime); override;
    function CalcValue(const Value1, Value2: T; const ALerp: single): T;
      virtual; abstract;
    function ValidValueString(const s: string): boolean; override;
  end;

  TAnimationVariantTrack = class abstract(
 {$IFDEF FPC}specialize{$ENDIF} TAnimationTrackGeneric<variant>)
  protected
    function KeyFrameValueToString(const AValue: variant): string; override;
    function KeyFrameValueFromString(const s: string): variant; override;
  public
    function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
      override;
  end;

  TAnimationVector2Track = class abstract(
 {$IFDEF FPC}specialize{$ENDIF} TAnimationTrackGeneric<TVector2>)
  protected
    function KeyFrameValueToString(const AValue: TVector2): string; override;
    function KeyFrameValueFromString(const s: string): TVector2; override;
    function SameKeyFrameValue(const Value1, Value2: TVector2): boolean; override;
  public
    function CalcValue(const Value1, Value2: TVector2; const ALerp: single): TVector2;
      override;
  end;

  TAnimationVector3Track = class abstract(
 {$IFDEF FPC}specialize{$ENDIF} TAnimationTrackGeneric<TVector3>)
  protected
    function KeyFrameValueToString(const AValue: TVector3): string; override;
    function KeyFrameValueFromString(const s: string): TVector3; override;
    function SameKeyFrameValue(const Value1, Value2: TVector3): boolean; override;
  public
    function CalcValue(const Value1, Value2: TVector3; const ALerp: single): TVector3;
      override;
  end;

  TAnimationVector4Track = class abstract(
 {$IFDEF FPC}specialize{$ENDIF} TAnimationTrackGeneric<TVector4>)
  protected
    function KeyFrameValueToString(const AValue: TVector4): string; override;
    function KeyFrameValueFromString(const s: string): TVector4; override;
    function SameKeyFrameValue(const Value1, Value2: TVector4): boolean; override;
  public
    function CalcValue(const Value1, Value2: TVector4; const ALerp: single): TVector4;
      override;
  end;

  TAnimationPropertyTrack = class(TAnimationVariantTrack)
  strict private
    function GetPropertyInfo: PPropInfo;
    procedure SetPersistent(const AValue: TPersistent);
  protected
    FPersistent: TPersistent;
    FProperty: string;
    FPropertyInfo: PPropInfo;
    procedure SetValue(const AValue: variant); override;
    procedure SetComponent(const AValue: TComponent); override;
    procedure SetFriendlyObjectName(const AValue: string); override;
    procedure UpdatePersistent;
  public
    constructor Create(APersistent: TPersistent; const AProperty: string); overload;
    function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
     override;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess;
     const APath: string; const bReading: boolean; const APlayer: TComponent); override;
    function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
    function ObjectName: string; override;
    function PropName: string; override;
    property Persistent: TPersistent read FPersistent write SetPersistent ;
    property PropertyName: string read FProperty;
    property PropertyInfo: PPropInfo read GetPropertyInfo;
  end;

  TAnimationTrackList = class(
    {$IFDEF FPC}specialize{$ENDIF} TObjectList<TAnimationTrack>)
  end;

  TAnimationPlayStyle = (apsOnce, apsLoop, apsPingPong, apsPingPongOnce);

  TAnimationPlayer = class;

  TAnimation = class
  strict private
    FMaxTime: TFloatTime;
    FOnComplete: TNotifyEvent;
    FOnTrackListChanged: TNotifyEvent;
    FPlayStyle: TAnimationPlayStyle;
    FTrackList: TAnimationTrackList;
    FCurrentTime: TFloatTime;
    FPlaying: boolean;
    FSpeed: single;
    procedure SetActualCurrentTime(const AValue: TFloatTime);
    procedure SetOnComplete(const AValue: TNotifyEvent);
    procedure SetOnTrackListChanged(const AValue: TNotifyEvent);
    procedure SetPlayStyle(const AValue: TAnimationPlayStyle);
    procedure TrackListNotify(ASender: TObject;
      {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationTrack; AAction: TCollectionNotification);
    procedure SetPlaying(const Value: boolean);
    procedure SetSpeed(const Value: single);
    function Loop: boolean;
    { Whenever KeyFrameList or TrackList changes, this function will be triggered.
      Then we update the value of FMaxTime.}
    procedure UpdateMaxTime;
    procedure TrackChange(Sender: TObject);
    function GetPingPongEvalTime: TFloatTime;
    procedure ResetLastExecutedKeyFrame;
  protected
    FPlayer: TAnimationPlayer;
    procedure Update(const DeltaTime: TFloatTime);
    procedure UpdateByCurrentTime(out bCompleted: boolean);
    procedure UpdateByTime(const ATime: TFloatTime);

    property OnComplete: TNotifyEvent read FOnComplete write SetOnComplete;
    property OnTrackListChanged: TNotifyEvent
      read FOnTrackListChanged write SetOnTrackListChanged;
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
    FOnAnimationListChanged: TNotifyEvent;
    FOnCurrentAnimationChanged: TNotifyEvent;
    FOnCurrentAnimationTrackListChanged: TNotifyEvent;
    FPlaying: boolean;
    procedure AAnimationTrackListChanged(Sender: TObject);
    procedure FAnimationListKeyNotify(ASender: TObject;
 {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: string; AAction: TCollectionNotification);
    procedure SetOnAnimationComplete(const AValue: TNotifyEvent);
    procedure SetOnAnimationListChanged(const AValue: TNotifyEvent);
    procedure SetOnCurrentAnimationChanged(const AValue: TNotifyEvent);
    procedure SetOnCurrentAnimationTrackListChanged(const AValue: TNotifyEvent);
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
    procedure RenameAnimation(const AOldName, ANewName: string);
    procedure ClearAnimations;
    procedure Start(const ResetTime: boolean = True);
    procedure Stop(const ResetTime: boolean = True);

    procedure CustomSerialization(const SerializationProcess: TSerializationProcess);
      override;

    property AnimationList: TAnimationList read FAnimationList;
    property CurrentAnimation: TAnimation read FCurrentAnimation;
  published
    property Animation: string read FAnimation write SetAnimation;
    property Playing: boolean read FPlaying write SetPlaying default True;
    property OnAnimationComplete: TNotifyEvent
      read FOnAnimationComplete write SetOnAnimationComplete;
    property OnCurrentAnimationChanged: TNotifyEvent
      read FOnCurrentAnimationChanged write SetOnCurrentAnimationChanged;
    property OnCurrentAnimationTrackListChanged: TNotifyEvent
      read FOnCurrentAnimationTrackListChanged write SetOnCurrentAnimationTrackListChanged;
    property OnAnimationListChanged: TNotifyEvent
      read FOnAnimationListChanged write SetOnAnimationListChanged;
  end;

function FloatMod(a, b: TFloatTime): TFloatTime;

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

uses Math, Generics.Defaults, RttiUtils;

{$define read_implementation}

{$I castleinternal_lerpfunctions.inc}

{$undef read_implementation}
const
  TLerpFuncArr: array[TLerpFuncType] of TLerpFunc =
    (nil, {$Ifdef fpc}@{$endif}LerpLiner, {$Ifdef fpc}@{$endif}LerpSin,
    {$Ifdef fpc}@{$endif}LerpElastic, {$Ifdef fpc}@{$endif}LerpBack,
    {$Ifdef fpc}@{$endif}LerpOneMinusCos,
    {$Ifdef fpc}@{$endif}LerpUniformDeceleration, {$Ifdef fpc}@{$endif}LerpQuad,
    {$Ifdef fpc}@{$endif}LerpCubic,{$Ifdef fpc}@{$endif}LerpLowWave,
    {$Ifdef fpc}@{$endif}LerpMiddleWave,{$Ifdef fpc}@{$endif}LerpHighWave,
    {$Ifdef fpc}@{$endif}LerpBounce,{$Ifdef fpc}@{$endif}LerpCircle);


function Vector2ToString(const AValue: TVector2): string;
begin
  Result := FormatDot('(%f,%f)', [AValue[0], AValue[1]]);
end;

function Vector2FromString(const s: string): TVector2;
var
  Tokens: {$Ifdef fpc}specialize{$endif}TArray<string>;
  Len: integer;
begin
  Tokens := S.Trim(['(', ')']).Split([',']);
  Len := Length(Tokens);
  if Len = 2 then
    Result := Vector2(StrToFloatDot(Tokens[0]), StrToFloatDot(Tokens[1]))
  else
    raise Exception.CreateFmt(
      'Conversion to TVector2 failed: "%s", length not match.', [s]);
end;

function Vector3ToString(const AValue: TVector3): string;
begin
  Result := FormatDot('(%f,%f,%f)', [AValue[0], AValue[1], AValue[2]]);
end;

function Vector3FromString(const s: string): TVector3;
var
  Tokens: {$Ifdef fpc}specialize{$endif}TArray<string>;
  Len: integer;
begin
  Tokens := S.Trim(['(', ')']).Split([',']);
  Len := Length(Tokens);
  if Len = 3 then
    Result := Vector3(StrToFloatDot(Tokens[0]), StrToFloatDot(Tokens[1]),
      StrToFloatDot(Tokens[2]))
  else
    raise Exception.CreateFmt(
      'Conversion to TVector3 failed: "%s", length not match.', [s]);
end;

function Vector4ToString(const AValue: TVector4): string;
begin
  Result := FormatDot('(%f,%f,%f,%f)', [AValue[0], AValue[1], AValue[2], AValue[3]]);
end;

function Vector4FromString(const s: string): TVector4;
var
  Tokens: {$Ifdef fpc}specialize{$endif}TArray<string>;
  Len: integer;
begin
  Tokens := S.Trim(['(', ')']).Split([',']);
  Len := Length(Tokens);
  if Len = 4 then
    Result := Vector4(StrToFloatDot(Tokens[0]), StrToFloatDot(Tokens[1]),
      StrToFloatDot(Tokens[2]), StrToFloatDot(Tokens[3]))
  else
    raise Exception.CreateFmt(
      'Conversion to TVector4 failed: "%s", length not match.', [s]);
end;

function FloatMod(a, b: TFloatTime): TFloatTime;
begin
  if b <= 0 then Exit(0);
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

function VariantToString(const V: variant): string;
var
  Len: integer;
begin
  if VarIsNull(v) then Exit('');
  if VarIsArray(V) then
  begin
    Len := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
    case Len of
      2: Result := Format('(%s,%s)', [V[0], V[1]]);
      3: Result := Format('(%s,%s,%s)', [V[0], V[1], V[2]]);
      4: Result := Format('(%s,%s,%s,%s)', [V[0], V[1], V[2], V[3]]);
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
  if Assigned(FComponent) then FComponent.RemoveFreeNotification(Self);
  FKeyframeList.Free;
  inherited Destroy;
end;

function TAnimationTrack.ObjectName: string;
begin
  if Assigned(FComponent) then Result := FComponent.Name
  else
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
      Frame := NewKeyFrame;
      AddKeyframe(Frame);
    end
    else
    begin
      Frame := KeyframeList[I];
    end;
    { KeyFrame propery }
    FramePath := KeyItem(KeyProp(APath, SFrame), I);

    Aint := Ord(Frame.LerpFuncType);
    SerializationProcess.ReadWriteInteger(
      KeyProp(FramePath, 'LerpFuncType'),
      Aint, Aint <> 0);
    Frame.LerpFuncType := TLerpFuncType(Aint);

    Afloat := Frame.Time;
    SerializationProcess.ReadWriteSingle(
      KeyProp(FramePath, 'Time'),
      Afloat, True);
    Frame.Time := Afloat;

    s := Frame.ValueToString;
    SerializationProcess.ReadWriteString(
      KeyProp(FramePath, 'Value'),
      s, s <> '');
    Frame.ValueFromString(s);
  end;

  //Get FComponent
  if Assigned(FComponent) then s := FComponent.Name
  else
    s := '';
  ComponentSerialization(SerializationProcess, self, 'Component', s, APath, bReading);

end;

function TAnimationTrack.AddKeyframe(
  const AValue: TAnimationKeyframe): TAnimationKeyframe;
begin
  AValue.OnChange := {$Ifdef fpc}@{$endif}KeyFramInTrackChange;
  FKeyframeList.Add(AValue);
  Result := AValue;
end;

function TAnimationTrack.RemoveKeyFrame(const AValue: TAnimationKeyframe): SizeInt;
begin
  Result := FKeyframeList.Remove(AValue);
end;

procedure TAnimationTrack.RemoveKeyFrame(AIndex: SizeInt);
begin
  Assert(Between(AIndex, 0, FKeyframeList.Count - 1));
  RemoveKeyFrame(FKeyframeList[AIndex]);
end;

function TAnimationTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  Result := False;
end;

procedure TAnimationTrackGeneric{$IFNDEF FPC}<T>{$ENDIF}.Evaluate(
  const ATime: TFloatTime);
var
  AValue: T;
  Index: SizeInt;
  AKeyFrame: TAnimationKeyframeInternal;
begin
  if FKeyframeList.Count = 0 then
    Exit;
  if ATime < FKeyframeList.First.Time then
  begin
    if (FLastExecutedKeyFrame <> nil) then
      FLastExecutedKeyFrame := nil;
    Exit;
  end;

  Index := FKeyframeList.TimeToKeyFrame(ATime);
  if Between(Index, 0, FKeyframeList.Count - 2) then
  begin
    AKeyFrame := FKeyframeList[Index] as TAnimationKeyframeInternal;
    AValue := Interpolate(AKeyFrame, FKeyframeList[Index + 1], ATime);
  end
  else
  begin
    { If the time is before the first frame or after the last frame, it's considered as the last static frame.}
    AKeyFrame := FKeyframeList.Last as TAnimationKeyframeInternal;
    AValue := AKeyFrame.Value;
  end;

  if (FMode = tmDiscrete) then
  begin
    if (FLastExecutedKeyFrame = AKeyFrame) then Exit
    else
      FLastExecutedKeyFrame := AKeyFrame;
  end;

  SetValue(AValue);
end;

function TAnimationTrackGeneric{$IFNDEF FPC}<T>{$ENDIF}.ValidValueString(
  const s: string): boolean;
var
  AValue: T;
begin
  try
    AValue := KeyFrameValueFromString(s);
    CalcValue(AValue, AValue, 0.5);
    Result := True;
  except
    Result := False;
  end;
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
  UpdateMaxTime;
  if Assigned(FOnTrackListChanged) then FOnTrackListChanged(Self);
end;

procedure TAnimation.SetOnComplete(const AValue: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnComplete), TMethod(AValue)) then
    FOnComplete := AValue;
end;

procedure TAnimation.SetOnTrackListChanged(const AValue: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnTrackListChanged), TMethod(AValue)) then
    FOnTrackListChanged := AValue;
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
  Track.FAnimation := Self;
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
begin
  EvalTime := 0;
  bCompleted := False;
  //delphi not support: FCurrentTime := FCurrentTime mod MaxTime
  case FPlayStyle of
    apsLoop:
    begin
      if FCurrentTime >= MaxTime then ResetLastExecutedKeyFrame;
      FCurrentTime := FloatMod(FCurrentTime, MaxTime);
      EvalTime := FCurrentTime;
    end;
    apsPingPong:
    begin
      if FCurrentTime >= 2 * MaxTime then ResetLastExecutedKeyFrame;
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
    FTrackList[I].Evaluate(EvalTime);
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
  begin
    ResetLastExecutedKeyFrame;
    FCurrentTime := 0;
  end;
  if not FPlaying then FPlaying := True;
end;

procedure TAnimation.UpdateMaxTime;
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
  UpdateMaxTime;
end;

function TAnimation.GetPingPongEvalTime: TFloatTime;
begin
  FCurrentTime := FloatMod(FCurrentTime, 2 * MaxTime);
  Result := FCurrentTime;
  if Result >= MaxTime then
    Result := 2 * MaxTime - Result;
end;

procedure TAnimation.ResetLastExecutedKeyFrame;
var
  ATrack: TAnimationTrack;
begin
  for ATrack in FTrackList do
    if ATrack.FMode = tmDiscrete then
      ATrack.FLastExecutedKeyFrame := nil;
end;

procedure TAnimationTrack.SetOnChange(const AValue: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnChange), TMethod(AValue)) then
    FOnChange := AValue;
end;

procedure TAnimationTrack.SetComponent(const AValue: TComponent);
begin
  if FComponent = AValue then Exit;
  if Assigned(FComponent) then FComponent.RemoveFreeNotification(Self);
  FComponent := AValue;
  if Assigned(FComponent) then FComponent.FreeNotification(Self);
end;

procedure TAnimationTrack.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    //FComponent destroyed, remove me
    if Assigned(FAnimation) then
    begin
      FAnimation.RemoveTrack(Self);
    end;
  end;
end;

function TAnimationTrack.GetFriendlyObjectName: string;
begin
  Result := FFriendlyObjectName;
  if Result = '' then Result := ObjectName;
end;

function CompareKeyframe({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif}
  Left, Right: TAnimationKeyframe): integer;
begin
  Result := CompareValue(Left.Time, Right.Time);
end;

constructor TAnimationTrack.Create(AOwner: TComponent);
type
  TInternalKeyframeComparer =
    {$IFDEF FPC}specialize{$ENDIF}TComparer<TAnimationKeyframe>;
begin
  if Assigned(AOwner) then raise Exception.Create('Owner should be nil.');
  inherited Create(nil);
  FMode := TAnimationTrackMode.tmContinuous;
  FKeyframeList := TAnimationKeyframeList.Create(TInternalKeyframeComparer.Construct(
    {$Ifdef fpc}@{$endif}CompareKeyframe), True);
  FKeyframeList.OnNotify :=
    {$Ifdef fpc}@{$endif}KeyframesNotify;
end;

procedure TAnimationTrack.SetFriendlyObjectName(const AValue: string);
begin
  if FFriendlyObjectName <> AValue then
    FFriendlyObjectName := AValue;
end;

constructor TAnimationTrack.Create;
begin
  Create(nil);
end;

procedure TAnimationTrack.KeyFramInTrackChange(ASender: TObject;
  const AChangeType: TKeyFrameChangeType);
begin
  case AChangeType of
    TKeyFrameChangeType.kfcTime:
    begin
      FKeyframeList.Sort;
      FLastExecutedKeyFrame := nil;
      if Assigned(FOnChange) then FOnChange(Self);
    end;
    TKeyFrameChangeType.kfcValue:
      FLastExecutedKeyFrame := nil;
    TKeyFrameChangeType.kfcLerpFunc: ;
  end;
end;

procedure TAnimationTrack.KeyframesNotify(ASender: TObject;
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: TAnimationKeyframe; AAction: TCollectionNotification);
begin
  FKeyframeList.Sort;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TAnimationTrackGeneric{$IFNDEF FPC}<T>{$ENDIF}.Interpolate(
  const Keyframe1, Keyframe2: TAnimationKeyframe; const Time: TFloatTime): T;
var
  ALerp: single;
  F1, F2: TAnimationKeyframeInternal;
begin
  F1 := Keyframe1 as TAnimationKeyframeInternal;
  F2 := Keyframe2 as TAnimationKeyframeInternal;
  case self.Mode of
    tmDiscrete: Result := F1.Value;
    tmContinuous:
    begin
      if F2.Time = F1.Time then
        ALerp := 0
      else
        ALerp := (Time - F1.Time) / (F2.Time - F1.Time);

      if IsNan(ALerp) or IsInfinite(ALerp) then
        raise Exception.Create('Floating point error');

      if Assigned(F1.LerpFunc) then ALerp := F1.LerpFunc(ALerp);
      Result := CalcValue(F1.Value, F2.Value, ALerp);
    end;
  end;
end;

function TAnimationTrackGeneric{$IFNDEF FPC}<T>{$ENDIF}.SameKeyFrameValue(
  const Value1, Value2: T): boolean;
begin
  Result := False;
end;

function TAnimationTrackGeneric{$IFNDEF FPC}<T>{$ENDIF}.NewKeyFrame: TAnimationKeyframe;
var
  AFrame: TAnimationKeyframeInternal;
begin
  AFrame := TAnimationKeyframeInternal.Create;
  AFrame.FOwner := Self;
  Result := AFrame;
end;

function TAnimationTrackGeneric{$IFNDEF FPC}<T>{$ENDIF}.AddKeyframe(
  const ATime: TFloatTime; const AValue: T;
  const ALerpFunc: TLerpFunc): TAnimationKeyframeInternal;
begin
  Result := NewKeyFrame as TAnimationKeyframeInternal;
  Result.Time := ATime;
  Result.Value := AValue;
  Result.LerpFunc := ALerpFunc;
  inherited AddKeyframe(Result);
end;

function TAnimationTrackGeneric{$IFNDEF FPC}<T>
{$ENDIF}.TAnimationKeyframeInternal.GetOwnerTrack: TAnimationTrack;
begin
  Result := FOwner;
end;

function TAnimationTrackGeneric{$IFNDEF FPC}<T>
{$ENDIF}.TAnimationKeyframeInternal.SameKeyFrameValue(
  const Value1, Value2: T): boolean;
begin
  if Assigned(FOwner) then
    Result := FOwner.SameKeyFrameValue(Value1, Value2)
  else
    Result := inherited SameKeyFrameValue(Value1, Value2);
end;

function TAnimationTrackGeneric{$IFNDEF FPC}<T>
{$ENDIF}.TAnimationKeyframeInternal.ValueToString: string;
begin
  if not Assigned(FOwner) then Exit;
  Result := FOwner.KeyFrameValueToString(Value);
end;

procedure TAnimationTrackGeneric{$IFNDEF FPC}<T>
{$ENDIF}.TAnimationKeyframeInternal.ValueFromString(const s: string);
begin
  if not Assigned(FOwner) then Exit;
  Value := FOwner.KeyFrameValueFromString(s);
end;

function TAnimationVariantTrack.KeyFrameValueToString(const AValue: variant): string;
begin
  Result := VariantToString(AValue);
end;

function TAnimationVariantTrack.KeyFrameValueFromString(const s: string): variant;
begin
  Result := VariantFromString(s);
end;

function TAnimationVariantTrack.CalcValue(const Value1, Value2: variant;
  const ALerp: single): variant;
var
  V1_int, V2_int: int64;
  V1_float, V2_float: extended;
begin
  if VarIsOrdinal(Value1) and (VarIsOrdinal(Value2)) then
  begin
    V1_int := Value1;
    V2_int := Value2;
    Result := Floor((1 - ALerp) * V1_int + ALerp * V2_int);
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

function TAnimationVector2Track.KeyFrameValueToString(const AValue: TVector2): string;
begin
  Result := Vector2ToString(AValue);
end;

function TAnimationVector2Track.KeyFrameValueFromString(const s: string): TVector2;
begin
  Result := Vector2FromString(s);
end;

function TAnimationVector2Track.SameKeyFrameValue(
  const Value1, Value2: TVector2): boolean;
begin
  Result := TVector2.Equals(Value1, Value2);
end;

function TAnimationVector2Track.CalcValue(const Value1, Value2: TVector2;
  const ALerp: single): TVector2;
begin
  Result := (1 - ALerp) * Value1 + ALerp * Value2;
end;

function TAnimationPropertyTrack.GetPropertyInfo: PPropInfo;
begin
  if not Assigned(FPropertyInfo) and Assigned(FPersistent) and (FProperty <> '') Then
    FPropertyInfo := GetPropInfo(FPersistent, FProperty);
  Result := FPropertyInfo;
end;

procedure TAnimationPropertyTrack.SetPersistent(const AValue: TPersistent);
begin
  if FPersistent = AValue then Exit;
  FPersistent := AValue;
end;

procedure TAnimationPropertyTrack.SetComponent(const AValue: TComponent
  );
begin
  if FComponent <> AValue then
  begin
    inherited;
    { Load FPersistent from FriendlyObjectName. }
    if not Assigned(Persistent) then UpdatePersistent;
  end;
end;

procedure TAnimationPropertyTrack.SetFriendlyObjectName(const AValue: string);
begin
  if FFriendlyObjectName <> AValue then
  begin
    inherited SetFriendlyObjectName(AValue);
    if not Assigned(Persistent) then UpdatePersistent;
  end;
end;

procedure TAnimationPropertyTrack.UpdatePersistent;

  function GetObject(AComponent:TPersistent; APropPath: string):TPersistent;

    function ChildObject(APersistent: TPersistent; const AName: string): TPersistent;
    var
      PropList: TPropInfoList;
      i: integer;
      PropValue: TObject;
    begin
      Result := nil;
      if not Assigned(APersistent) then Exit;

      PropList := TPropInfoList.Create(APersistent, tkProperties);
      try
        for i := 0 to PropList.Count - 1 do
        begin
          if (PropList[i]^.PropType^.Kind = tkClass) and (PropList[i]^.Name = AName) then
          begin
            PropValue := GetObjectProp(APersistent, PropList[i]);
            if PropValue is TPersistent then Exit(PropValue as TPersistent);
          end;
        end;
      finally
        PropList.Free;
      end;
    end;

  var
    arr:{$Ifdef fpc}specialize{$endif} TArray<string>;
    i:integer;
  begin
    if not Assigned(AComponent) then Exit(nil);
    arr := APropPath.Split(['.']);
    { arr[0] is AComponent's name. }
    if Length(arr) > 1 then
    begin
      for i := 1 to length(arr) -1 do
      begin
        AComponent := ChildObject(AComponent, arr[i]);
        if not Assigned(AComponent) then Break;
      end;
    end;
    Result := AComponent;
  end;

begin
  Persistent := GetObject(FComponent, FriendlyObjectName);
end;

procedure TAnimationPropertyTrack.SetValue(const AValue: variant);
begin
  if Assigned(FPersistent) and Assigned(PropertyInfo) then
    SetPropValue(FPersistent, FPropertyInfo, AValue);
end;

function TAnimationPropertyTrack.CalcValue(const Value1, Value2: variant;
  const ALerp: single): variant;
var
  V1_int, V2_int: int64;
  V1_float, V2_float, F: extended;
  Tk: TTypeKind;
begin
  if not Assigned(PropertyInfo) then Exit;

  Tk := FPropertyInfo^.PropType^.Kind;
  case Tk of
    tkInteger, tkInt64, tkEnumeration,
    tkSet, tkChar, tkWChar{$ifdef FPC}, tkBool{$endif}:
    begin
      V1_int := Value1;
      V2_int := Value2;
      F := (1 - ALerp) * V1_int + ALerp * V2_int;
      if V1_int <= V2_int then
        Result := Floor(F)
      else
        Result := Ceil(F);
    end;
    tkFloat:
    begin
      V1_float := Value1;
      V2_float := Value2;
      Result := (1 - ALerp) * V1_float + ALerp * V2_float;
    end;
    {$ifdef FPC}
    tkSString, tkLString, tkAString, tkWString:
    {$else}
    tkString, tkLString, tkUString, tkWString:
    {$endif}
      Result := Value1;
    else
      raise Exception.CreateFmt(
        'TAnimationPropertyTrack.SetValue: Unsupported value type[%d], Property:%s.',
        [Ord(Tk), FProperty]);
  end;

end;

constructor TAnimationPropertyTrack.Create(APersistent: TPersistent;
  const AProperty: string);
begin
  Create;
  Persistent := APersistent;
  FProperty := AProperty;
  FPropertyInfo := GetPropInfo(FPersistent, FProperty);

  if not Assigned(FPropertyInfo) then
    raise Exception.CreateFmt('%s does not exist in %s',
      [FProperty, FPersistent.ClassName])
  else
  begin
    case FPropertyInfo^.PropType^.Kind of
      {$ifdef FPC}
      tkSString, tkLString, tkAString, tkWString:
      {$else}
      tkString, tkLString, tkUString, tkWString:
      {$endif}
        FMode := tmDiscrete
      else;
    end;
  end;
end;

procedure TAnimationPropertyTrack.CustomSerialization(
  const SerializationProcess: TSerializationProcess; const APath: string;
  const bReading: boolean; const APlayer: TComponent);
var
  s: string;
  Tokens: {$Ifdef fpc}specialize{$endif}TArray<string>;
begin
  s := FriendlyObjectName;
  SerializationProcess.ReadWriteString(
    KeyProp(APath, 'FriendlyObjectName'),
    s, s <> '');
  if bReading and (s <> '') then
  begin
    FriendlyObjectName := s;
    //Get FPersistent
    s := '';
    SerializationProcess.ReadWriteString(KeyProp(APath, 'Component'), s, s <> '');
    if (s = '') then
    begin
      Tokens := FriendlyObjectName.Split(['.']);
      if Length(Tokens) < 1 then
        WritelnWarning('Canot serialize FriendlyObjectName, empty value.')
      else
        SerializationProcess.RequireComponent(self, GetPropInfo(self, 'Component'), Tokens[0]);
    end;
  end;

  s := FProperty;
  SerializationProcess.ReadWriteString(
    KeyProp(APath, 'PropertyName'),
    s, s <> '');
  if bReading then FProperty := s;

  inherited;
end;

function TAnimationPropertyTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(FPersistent) then
    Exit(inherited AddKeyframeAtTime(ATime, ALerpFunc));

  Result := Assigned(FPropertyInfo);
  if Result then  AddKeyframe(ATime, GetPropValue(FPersistent, FPropertyInfo), ALerpFunc)
  else WritelnWarning('PropertyInfo is nil.');
end;

function TAnimationPropertyTrack.ObjectName: string;
begin
  if not Assigned(FPersistent) then
    Result := inherited
  else if FPersistent is TComponent then
    Result := (FPersistent as TComponent).Name
  else
    Result := '(' + FPersistent.ClassName + ')';

end;

function TAnimationPropertyTrack.PropName: string;
begin
  Result := FProperty;
end;

procedure TAnimationKeyframe.SetTime(const AValue: TFloatTime);
begin
  if FTime = AValue then Exit;
  FTime := AValue;
  Changed(kfcTime);
end;

procedure TAnimationKeyframe.Changed(const AChangeType: TKeyFrameChangeType);
begin
  if Assigned(FOnChange) then FOnChange(Self, AChangeType);
end;

procedure TAnimationKeyframe.SetOnChange(const AValue: TKeyFrameChangeEvent);
begin
  if not SameMethods(TMethod(FOnChange), TMethod(AValue)) then
    FOnChange := AValue;
end;

procedure TAnimationKeyframe.SetLerpFunc(const AValue: TLerpFunc);
begin
  //TODO: delphi fail : if FLerpFunc = AValue then Exit;
  FLerpFunc := AValue;
  FLerpFuncType := lftCustom;
  Changed(kfcLerpFunc);
end;

procedure TAnimationKeyframe.SetLerpFuncType(const AValue: TLerpFuncType);
begin
  if FLerpFuncType <> AValue then
  begin
    FLerpFuncType := AValue;
    if FLerpFuncType <> lftCustom then
      FLerpFunc := TLerpFuncArr[FLerpFuncType];
    Changed(kfcLerpFunc);
  end;
end;

procedure TAnimationKeyframeGeneric{$IFNDEF FPC}<T>{$ENDIF}.SetValue(const AValue: T);
begin
  if SameKeyFrameValue(FValue, AValue) then Exit;
  FValue := AValue;
  Changed(kfcValue);
end;

function TAnimationKeyframeGeneric{$IFNDEF FPC}<T>{$ENDIF}.SameKeyFrameValue(
  const Value1, Value2: T): boolean;
begin
  Result := False;
end;

function TAnimationKeyframeList.SearchIndex(const ATime: TFloatTime): SizeInt;

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

function TAnimationKeyframeList.TimeToKeyFrame(const ATime: TFloatTime): SizeInt;
begin
  Result := SearchIndex(ATime) - 1;
end;

function TAnimationVector3Track.KeyFrameValueToString(const AValue: TVector3): string;
begin
  Result := Vector3ToString(AValue);
end;

function TAnimationVector3Track.KeyFrameValueFromString(const s: string): TVector3;
begin
  Result := Vector3FromString(s);
end;

function TAnimationVector3Track.SameKeyFrameValue(
  const Value1, Value2: TVector3): boolean;
begin
  Result := TVector3.Equals(Value1, Value2);
end;

function TAnimationVector3Track.CalcValue(const Value1, Value2: TVector3;
  const ALerp: single): TVector3;
begin
  Result := (1 - ALerp) * Value1 + ALerp * Value2;
end;

function TAnimationVector4Track.KeyFrameValueToString(const AValue: TVector4): string;
begin
  Result := Vector4ToString(AValue);
end;

function TAnimationVector4Track.KeyFrameValueFromString(const s: string): TVector4;
begin
  Result := Vector4FromString(s);
end;

function TAnimationVector4Track.SameKeyFrameValue(
  const Value1, Value2: TVector4): boolean;
begin
  Result := TVector4.Equals(Value1, Value2);
end;

function TAnimationVector4Track.CalcValue(const Value1, Value2: TVector4;
  const ALerp: single): TVector4;
begin
  Result := (1 - ALerp) * Value1 + ALerp * Value2;
end;

procedure TAnimation.Stop(const ResetTime: boolean);
begin
  if ResetTime then
  begin
    ResetLastExecutedKeyFrame;
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

procedure TAnimationPlayer.SetOnAnimationListChanged(const AValue: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnAnimationListChanged), TMethod(AValue)) then
    FOnAnimationListChanged := AValue;
end;

procedure TAnimationPlayer.AAnimationTrackListChanged(Sender: TObject);
begin
  if (Sender = CurrentAnimation) and Assigned(FOnCurrentAnimationTrackListChanged) then
    FOnCurrentAnimationTrackListChanged(Self);
end;

procedure TAnimationPlayer.FAnimationListKeyNotify(ASender: TObject;
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AItem: string; AAction: TCollectionNotification);
begin
  if Assigned(FOnAnimationListChanged) then FOnAnimationListChanged(Self);
end;

procedure TAnimationPlayer.SetOnCurrentAnimationChanged(const AValue: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnCurrentAnimationChanged), TMethod(AValue)) then
    FOnCurrentAnimationChanged := AValue;
end;

procedure TAnimationPlayer.SetOnCurrentAnimationTrackListChanged(
  const AValue: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnCurrentAnimationTrackListChanged), TMethod(AValue)) then
    FOnCurrentAnimationTrackListChanged := AValue;
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
      { "Animation" is a published property, it is normal that it does not exist
        during the first deserialization. }
      if not (self.IsLoading) then
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
  AAnimation.FPlayer := Self;
  AAnimation.OnComplete := {$Ifdef fpc}@{$endif}InternalAnimationComplete;
  AAnimation.OnTrackListChanged := {$Ifdef fpc}@{$endif}AAnimationTrackListChanged;
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
  { not ownvalues. wo need rename animation. }
  FAnimationList := TAnimationList.Create([]);
  FAnimationList.OnKeyNotify := {$Ifdef fpc}@{$endif}FAnimationListKeyNotify;
  FPlaying := True;
end;

destructor TAnimationPlayer.Destroy;
begin
  ClearAnimations;
  FreeAndNil(FAnimationList);
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
var
  Ani: TAnimation;
begin
  if FAnimationList.TryGetValue(AName, Ani) then
  begin
    if Animation = AName then
      Animation := '';
    FAnimationList.Remove(AName);
    FreeAndNil(Ani);
    //if FAnimationList.ContainsKey(AName) then
    //  WritelnWarning('RemoveAnimation: failed to remove animation "%s"', [AName]);
  end
  else
    WritelnWarning('RemoveAnimation: animation "%s" not exist.', [AName]);
end;

procedure TAnimationPlayer.RenameAnimation(const AOldName, ANewName: string);
var
  Ani: TAnimation;
  bCurrent: boolean;
begin
  if AOldName = ANewName then
    WritelnWarning('RenameAnimation: same names "%s", ignored.', [AOldName])
  else
  if AnimationList.TryGetValue(AOldName, Ani) then
  begin
    EnsureAnimationNameUnique(ANewName);
    bCurrent := Animation = AOldName;
    FAnimationList.Remove(AOldName);
    AddAnimation(ANewName, Ani);
    if bCurrent then Animation := ANewName;
  end
  else
    WritelnWarning('RenameAnimation: animation "%s" not exist.', [AOldName]);
end;

procedure TAnimationPlayer.ClearAnimations;
var
  Ani: TAnimation;
begin
  Animation := '';
  for Ani in FAnimationList.Values do Ani.Free;
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

initialization
  RegisterClasses([TAnimationPropertyTrack]);
end.
