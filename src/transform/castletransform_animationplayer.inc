{%MainUnit castletransform.pas}
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

{$ifdef read_interface}
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

  TAnimationTranslationTrack = class(TAnimationVector3Track)
  private
    function GetTransform: TCastleTransform;
    procedure SetTransform(const AValue: TCastleTransform);
  protected
    procedure SetValue(const AValue: TVector3); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function ObjectName: string; override;
    function PropName: string; override;
    function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
  published
    property Transform: TCastleTransform read GetTransform write SetTransform;
  end;

  TAnimationRotationTrack = class(TAnimationVector4Track)
  private
    function GetTransform: TCastleTransform;
    procedure SetTransform(const AValue: TCastleTransform);
  protected
    procedure SetValue(const AValue: TVector4); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function ObjectName: string; override;
    function PropName: string; override;
    function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
  published
    property Transform: TCastleTransform read GetTransform write SetTransform;
  end;

  TAnimationScaleTrack = class(TAnimationVector3Track)
  private
    function GetTransform: TCastleTransform;
    procedure SetTransform(const AValue: TCastleTransform);
  protected
    procedure SetValue(const AValue: TVector3); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function ObjectName: string; override;
    function PropName: string; override;
    function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
  published
    property Transform: TCastleTransform read GetTransform write SetTransform;
  end;

  TCastleAnimationPlayerTransform = class(TCastleTransform)
  strict private
    FAnimationPlayer: TAnimationPlayer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    property AnimationPlayer: TAnimationPlayer read FAnimationPlayer;
  end;

{$endif read_interface}

{$ifdef read_implementation}
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
  //TODO: FPersistent freenotify
end;

procedure TAnimationPropertyTrack.SetComponent(const AValue: TComponent
  );

  function ChildObject(Persistent: TPersistent; const AName: string): TPersistent;
  var
    PropList: TPropInfoList;
    i: integer;
    PropValue: TObject;
  begin
    Result := nil;
    PropList := TPropInfoList.Create(Persistent, tkProperties);
    try
      for i := 0 to PropList.Count - 1 do
      begin
        if (PropList[i]^.PropType^.Kind = tkClass) and (PropList[i]^.Name = AName) then
        begin
          PropValue := GetObjectProp(Persistent, PropList[i]);
          if PropValue is TPersistent then Exit(PropValue as TPersistent);
        end;
      end;
    finally
      PropList.Free;
    end;
  end;

  function GetObject(AComponent:TPersistent; APropPath: string):TPersistent;
  var
    arr:{$Ifdef fpc}specialize{$endif} TArray<string>;
    i:integer;
  begin
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
  if FComponent <> AValue then
  begin
    inherited;
    if not Assigned(FComponent) then Exit;
    { Load FPersistent from FriendlyObjectName. }
    Persistent := GetObject(FComponent, FriendlyObjectName);
  end;
end;

{ TAnimationPropertyTrack --------------------------------------------------------------- }
procedure TAnimationPropertyTrack.SetValue(const AValue: variant);
begin
  if Assigned(FPersistent) and Assigned(PropertyInfo) then
    SetPropValue(FPersistent, FPropertyInfo, AValue);
end;

function TAnimationPropertyTrack.CalcValue(const Value1, Value2: variant;
  const ALerp: single): variant;
var
  V1_int, V2_int: int64;
  V1_float, V2_float: extended;
  Tk: TTypeKind;
begin
  if not Assigned(PropertyInfo) then Exit;

  Tk := FPropertyInfo^.PropType^.Kind;
  case Tk of
    tkInteger, tkInt64, tkEnumeration,
    tkSet, tkChar, tkWChar:
    begin
      V1_int := Value1;
      V2_int := Value2;
      Result := Floor((1 - ALerp) * V1_int + ALerp * V2_int);
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
  Result := inherited;
  if not Assigned(FPersistent) then exit;
  if FPersistent is TComponent then
    Result := (FPersistent as TComponent).Name
  else
    Result := '(' + FPersistent.ClassName + ')';

end;

function TAnimationPropertyTrack.PropName: string;
begin
  Result := FProperty;
end;

function TAnimationTranslationTrack.GetTransform: TCastleTransform;
begin
  Result := TCastleTransform(FComponent);
end;

procedure TAnimationTranslationTrack.SetTransform(const AValue: TCastleTransform
  );
begin
  SetComponent(AValue);
end;

{ TAnimationTranslationTrack --------------------------------------------------------------- }
procedure TAnimationTranslationTrack.SetValue(const AValue: TVector3);
begin
  Transform.Translation := AValue;
end;

constructor TAnimationTranslationTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  Transform := ATransform;
end;

function TAnimationTranslationTrack.ObjectName: string;
begin
  if Assigned(Transform) then Result := Transform.Name
  else
    Result := '';
end;

function TAnimationTranslationTrack.PropName: string;
begin
  Result := 'Translation';
end;

function TAnimationTranslationTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(Transform) then Exit(inherited);
  AddKeyframe(ATime, Transform.Translation, ALerpFunc);
  Result :=True;
end;

function TAnimationRotationTrack.GetTransform: TCastleTransform;
begin
  Result := TCastleTransform(FComponent);
end;

procedure TAnimationRotationTrack.SetTransform(const AValue: TCastleTransform);
begin
  SetComponent(AValue);
end;

{ TAnimationRotationTrack --------------------------------------------------------------- }
procedure TAnimationRotationTrack.SetValue(const AValue: TVector4);
begin
  if Assigned(Transform) then
    Transform.Rotation := AValue;
end;

constructor TAnimationRotationTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  Transform := ATransform;
end;

function TAnimationRotationTrack.ObjectName: string;
begin
  if Assigned(Transform) then Result := Transform.Name
  else Result := '';
end;

function TAnimationRotationTrack.PropName: string;
begin
  Result := 'Rotation';
end;

function TAnimationRotationTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(Transform) then Exit(inherited);
  AddKeyframe(ATime, Transform.Rotation, ALerpFunc);
  Result :=True;
end;

function TAnimationScaleTrack.GetTransform: TCastleTransform;
begin
  Result := TCastleTransform(FComponent);
end;

procedure TAnimationScaleTrack.SetTransform(const AValue: TCastleTransform);
begin
    SetComponent(AValue);
end;

{ TAnimationScaleTrack --------------------------------------------------------------- }
procedure TAnimationScaleTrack.SetValue(const AValue: TVector3);
begin
  Transform.Scale := AValue;
end;

constructor TAnimationScaleTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  Transform := ATransform;
end;

function TAnimationScaleTrack.ObjectName: string;
begin
  if Assigned(Transform) then Result := Transform.Name
  else Result := '';
end;

function TAnimationScaleTrack.PropName: string;
begin
  Result := 'Scale';
end;

function TAnimationScaleTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(Transform) then Exit(inherited);
  AddKeyframe(ATime, Transform.Scale, ALerpFunc);
  Result :=True;
end;

{ TCastleAnimationPlayerTransform --------------------------------------------------------------- }
constructor TCastleAnimationPlayerTransform.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimationPlayer := TAnimationPlayer.Create(Self);
  FAnimationPlayer.SetSubComponent(True);
end;

procedure TCastleAnimationPlayerTransform.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  FAnimationPlayer.Update(SecondsPassed);
end;

function TCastleAnimationPlayerTransform.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, ['AnimationPlayer']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
