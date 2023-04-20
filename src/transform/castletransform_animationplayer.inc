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
  TAnimationPropertyTrack = class(TAnimationTrack)
  strict private
    FInternalComponent: TComponent;
    function GetPropertyInfo: PPropInfo;
    procedure SetInternalComponent(const AValue: TComponent);
  strict protected
   FComponent: TPersistent;
   FProperty: string;
   FPropertyInfo: PPropInfo;
   procedure SetValue(const AValue: variant); override;
   function CalcValue(const Value1, Value2: variant; const ALerp: single): variant;
     override;
  public
   constructor Create(AComponent: TPersistent; const AProperty: string); overload;
   procedure CustomSerialization(const SerializationProcess: TSerializationProcess;
     const APath: string; const bReading: boolean; const APlayer: TComponent); override;
   function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
   function ObjectName: string; override;
   function PropName: string; override;
   property Component: TPersistent read FComponent;
   property PropertyName: string read FProperty;
   property PropertyInfo: PPropInfo read GetPropertyInfo;
  published
   property InternalComponent: TComponent read FInternalComponent write SetInternalComponent ;
  end;

  TAnimationTranslationTrack = class(TAnimationVector3Track)
  strict private
    FTransform: TCastleTransform;
  strict protected
    procedure SetValue(const AValue: variant); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function ObjectName: string; override;
    function PropName: string; override;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess;
      const APath: string; const bReading: boolean; const APlayer: TComponent);
      override;
    function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
  published
    property Transform: TCastleTransform read FTransform write FTransform;
  end;

  TAnimationRotationTrack = class(TAnimationVector4Track)
  strict private
    FTransform: TCastleTransform;
  strict protected
    procedure SetValue(const AValue: variant); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function ObjectName: string; override;
    function PropName: string; override;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess;
      const APath: string; const bReading: boolean; const APlayer: TComponent);
      override;
    function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
  published
    property Transform: TCastleTransform read FTransform write FTransform;
  end;

  TAnimationScaleTrack = class(TAnimationVector3Track)
  strict private
    FTransform: TCastleTransform;
  strict protected
    procedure SetValue(const AValue: variant); override;
  public
    constructor Create(ATransform: TCastleTransform);
    function ObjectName: string; override;
    function PropName: string; override;
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess;
      const APath: string; const bReading: boolean; const APlayer: TComponent);
      override;
    function AddKeyframeAtTime(const ATime: TFloatTime;
     const ALerpFunc: TLerpFunc = nil): boolean; override;
  published
    property Transform: TCastleTransform read FTransform write FTransform;
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
const
  STransform = 'Transform';

function TAnimationPropertyTrack.GetPropertyInfo: PPropInfo;
begin
  if not Assigned(FPropertyInfo) and Assigned(FComponent) and (FProperty <> '') Then
    FPropertyInfo := GetPropInfo(FComponent, FProperty);
  Result := FPropertyInfo;
end;

procedure TAnimationPropertyTrack.SetInternalComponent(const AValue: TComponent
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
    s:string;
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
  if FInternalComponent <> AValue then
  begin
    FInternalComponent := AValue;
    if not Assigned(FInternalComponent) then Exit;
    { Load FComponent from FriendlyObjectName. }
    FComponent := GetObject(FInternalComponent, FriendlyObjectName);
  end;
end;

{ TAnimationPropertyTrack --------------------------------------------------------------- }
procedure TAnimationPropertyTrack.SetValue(const AValue: variant);
begin
  if Assigned(FComponent) and Assigned(PropertyInfo) then
    SetPropValue(FComponent, FPropertyInfo, AValue);
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

constructor TAnimationPropertyTrack.Create(AComponent: TPersistent;
  const AProperty: string);
begin
  Create;
  FComponent := AComponent;
  FProperty := AProperty;
  FPropertyInfo := GetPropInfo(FComponent, FProperty);

  if not Assigned(FPropertyInfo) then
    raise Exception.CreateFmt('%s does not exist in %s',
      [FProperty, FComponent.ClassName])
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
  comp: TComponent;
  ARoot: TCastleUserInterface;
begin
  inherited;
  s := FriendlyObjectName;
  SerializationProcess.ReadWriteString(
    KeyProp(APath, 'FriendlyObjectName'),
    s, s <> '');
  if bReading and ( s<>'' ) then
  begin
    FriendlyObjectName := s;
    Tokens := s.Split(['.']);
    if Length(Tokens) < 1 then
    begin
      WritelnWarning('Canot serialize FriendlyObjectName, empty value.');
    end
    else
    begin
      //Get FComponent
      SerializationProcess.RequireComponent(self, GetPropInfo(self, 'InternalComponent'), Tokens[0]);
    end;
  end;

  s := FProperty;
  SerializationProcess.ReadWriteString(
    KeyProp(APath, 'PropertyName'),
    s, s <> '');
  if bReading then FProperty := s;
end;

function TAnimationPropertyTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(FComponent) then
    Exit(inherited AddKeyframeAtTime(ATime, ALerpFunc));

  Result := Assigned(FPropertyInfo);
  if Result then  AddKeyframe(ATime, GetPropValue(FComponent, FPropertyInfo), ALerpFunc)
  else WritelnWarning('PropertyInfo is nil.');
end;

function TAnimationPropertyTrack.ObjectName: string;
begin
  Result := inherited;
  if not Assigned(FComponent) then exit;
  if FComponent is TComponent then
    Result := (FComponent as TComponent).Name
  else
    Result := '(' + FComponent.ClassName + ')';

end;

function TAnimationPropertyTrack.PropName: string;
begin
  Result := FProperty;
end;

{ TAnimationTranslationTrack --------------------------------------------------------------- }
procedure TAnimationTranslationTrack.SetValue(const AValue: variant);
begin
  FTransform.Translation := VariantToVector3(AValue);
end;

constructor TAnimationTranslationTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  FTransform := ATransform;
end;

function TAnimationTranslationTrack.ObjectName: string;
begin
  if Assigned(FTransform) then Result := FTransform.Name
  else
    Result := '';
end;

function TAnimationTranslationTrack.PropName: string;
begin
  Result := 'Translation';
end;

procedure TAnimationTranslationTrack.CustomSerialization(
  const SerializationProcess: TSerializationProcess; const APath: string;
  const bReading: boolean; const APlayer: TComponent);
begin
  inherited;
  //Get FTransform
  ComponentSerialization(SerializationProcess, self, STransform, ObjectName, APath, bReading);
end;

function TAnimationTranslationTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(FTransform) then Exit(inherited);
  AddKeyframe(ATime, FTransform.Translation, ALerpFunc);
  Result :=True;
end;

{ TAnimationRotationTrack --------------------------------------------------------------- }
procedure TAnimationRotationTrack.SetValue(const AValue: variant);
begin
  if Assigned(FTransform) then
    FTransform.Rotation := VariantToVector4(AValue);
end;

constructor TAnimationRotationTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  FTransform := ATransform;
end;

function TAnimationRotationTrack.ObjectName: string;
begin
  if Assigned(FTransform) then Result := FTransform.Name
  else Result := '';
end;

function TAnimationRotationTrack.PropName: string;
begin
  Result := 'Rotation';
end;

procedure TAnimationRotationTrack.CustomSerialization(
  const SerializationProcess: TSerializationProcess; const APath: string;
  const bReading: boolean; const APlayer: TComponent);
begin
  inherited;
  //Get FTransform
  ComponentSerialization(SerializationProcess, self, STransform, ObjectName, APath, bReading);
end;

function TAnimationRotationTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(FTransform) then Exit(inherited);
  AddKeyframe(ATime, FTransform.Rotation, ALerpFunc);
  Result :=True;
end;

{ TAnimationScaleTrack --------------------------------------------------------------- }
procedure TAnimationScaleTrack.SetValue(const AValue: variant);
begin
  FTransform.Scale := VariantToVector3(AValue);
end;

constructor TAnimationScaleTrack.Create(ATransform: TCastleTransform);
begin
  inherited Create;
  FTransform := ATransform;
end;

function TAnimationScaleTrack.ObjectName: string;
begin
  if Assigned(FTransform) then Result := FTransform.Name
  else Result := '';
end;

function TAnimationScaleTrack.PropName: string;
begin
  Result := 'Scale';
end;

procedure TAnimationScaleTrack.CustomSerialization(
  const SerializationProcess: TSerializationProcess; const APath: string;
  const bReading: boolean; const APlayer: TComponent);
begin
  inherited;
  //Get FTransform
  ComponentSerialization(SerializationProcess, self, STransform, ObjectName, APath, bReading);
end;

function TAnimationScaleTrack.AddKeyframeAtTime(const ATime: TFloatTime;
  const ALerpFunc: TLerpFunc): boolean;
begin
  if not Assigned(FTransform) then Exit(inherited);
  AddKeyframe(ATime, FTransform.Scale, ALerpFunc);
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
