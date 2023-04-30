{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2023 Michalis Kamburelis, Freedomax.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
type
  TCastleScrollBarType = (sbHorizontal, sbVertical);

  TCastleScrollBar = class(TCastleUserInterface)
  private
    FFrame: TCastleImagePersistent;
    FSlider: TCastleImagePersistent;
    FScrollBarType: TCastleScrollBarType;
    FViewSize, FContentSize: Single;
    FPosition: Single;
    FDragOffset :single;
    FOnScroll: TNotifyEvent;
    FSliderColor: TCastleColor;
    FSliderAlpha: single;
    FSliderMinSize, FSliderMargin: Single;
    FHidden: Boolean;
    FDragging: Boolean;
    procedure SetContentSize(const AValue: Single);
    procedure SetViewSize(const AValue: Single);
    procedure UpdatePosition;
  protected
    procedure UIScaleChanged; override;
    function SliderRect: TFloatRectangle;
    function SliderSize: Single;
    procedure Scroll(const AValue: Single);
    procedure ScrollTo(AValue: Single);
    procedure ScrollBy(const AValue: Single);
    function MousePosToPosition(const APos: TVector2): Single;
    function PosValueToPosition(AValue: Single): Single;
    procedure UpdateVisibility;
    function GetSize:single;
    function GetPos(const APos:TVector2):single;
    function MouseDown(const  APos: TVector2): Boolean;
    function MouseMove(const APos: TVector2): Boolean;
    function MouseUp(const  APos: TVector2): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure SetContentAndViewSize(const AContentSize, AViewSize: Single);
    function PropertySections(const PropertyName: String): TPropertySections; override;

    //TODO: SliderXxx not used
    property SliderColor: TCastleColor read FSliderColor write FSliderColor;
    property SliderAlpha: Single read FSliderAlpha write FSliderAlpha;
    property SliderMinSize: Single read FSliderMinSize write FSliderMinSize;
    property SliderMargin: Single read FSliderMargin write FSliderMargin;

    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
  published
    property ViewSize: Single read FViewSize write SetViewSize{$Ifdef fpc} default 10.0{$endif};
    property ContentSize: Single read FContentSize write SetContentSize{$Ifdef fpc} default 100.0{$endif};
    property Position: Single read FPosition write ScrollTo{$Ifdef fpc} default 0.0{$endif};
    property ScrollBarType: TCastleScrollBarType read FScrollBarType write FScrollBarType default sbHorizontal;
    property Frame: TCastleImagePersistent read FFrame;
    property Slider: TCastleImagePersistent read FSlider;
  end;


{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  FViewSize := 10.0;
  FContentSize := 100.0;
  FPosition := 0.0;
  FScrollBarType := sbHorizontal;

  FSliderColor := White;
  FSliderAlpha := 1;
  FSliderMinSize := 8;
  FSliderMargin := 2;

  FHidden := False;
  FDragging := False;
  FFrame := TCastleImagePersistent.Create;
  FSlider := TCastleImagePersistent.Create;
end;

destructor TCastleScrollBar.Destroy;
begin
  FreeAndNil(FFrame);
  FreeAndNil(FSlider);
  inherited Destroy;
end;

procedure TCastleScrollBar.UpdatePosition;
begin
  FPosition := Clamped(FPosition, 0, FContentSize - FViewSize);
end;

procedure TCastleScrollBar.SetContentSize(const AValue: Single);
begin
  if FContentSize = AValue then Exit;
  FContentSize := AValue;
  UpdatePosition;
  UpdateVisibility;
end;

procedure TCastleScrollBar.SetViewSize(const AValue: Single);
begin
  if FViewSize = AValue then Exit;
  FViewSize := AValue;
  UpdatePosition;
  UpdateVisibility;
end;

procedure TCastleScrollBar.UIScaleChanged;
begin
  inherited;
  UpdateVisibility;
end;

procedure TCastleScrollBar.Resize;
begin
  inherited;
  UpdateVisibility;
end;

procedure TCastleScrollBar.Render;
var
  R: TFloatRectangle;

  procedure DrawImageType(const AImage: TCastleImagePersistent;
    const AThemeImage: TThemeImage; const ARect: TFloatRectangle);
  var
    FinalFrame: TCastleImagePersistent;
  begin
    if AImage.Empty then
      FinalFrame := Theme.ImagesPersistent[AThemeImage]
    else
      FinalFrame := AImage;

    FinalFrame.DrawUiBegin(UIScale);
    //TODO: color fade
    FinalFrame.Color := FinalFrame.Color;//TODO: color fade * Color;
    FinalFrame.Draw(ARect);
    FinalFrame.DrawUiEnd;
  end;

  procedure DrawSlider;
  var
    RSlider: TFloatRectangle;
  begin
    RSlider := SliderRect;
    RSlider := FloatRectangle(RSlider.LeftBottom * UIScale, RSlider.Width * UIScale,
      RSlider.Height * UIScale);
    RSlider.LeftBottom := R.LeftBottom + RSlider.LeftBottom;

    DrawImageType(FSlider, tiScrollbarSlider, RSlider);
  end;

  procedure DrawFrame;
  begin
    DrawImageType(FFrame, tiScrollbarFrame, R);
  end;

begin
  inherited;
  R := RenderRect;
  DrawFrame;
  DrawSlider;
end;

function TCastleScrollBar.Press(const Event: TInputPressRelease): boolean;
var
  APos: TVector2;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    APos := ContainerToLocalPosition(event.Position);
    Result := MouseDown(APos);
  end;
end;

function TCastleScrollBar.Release(const Event: TInputPressRelease): boolean;
var
  APos: TVector2;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    APos := ContainerToLocalPosition(event.Position);
    Result := MouseUp(APos);
  end;
end;

function TCastleScrollBar.Motion(const Event: TInputMotion): boolean;
var
  APos: TVector2;
begin
  Result := inherited;
  if Result then Exit;

  APos := ContainerToLocalPosition(Event.Position);
  Result := MouseMove(APos);
end;

procedure TCastleScrollBar.SetContentAndViewSize(const AContentSize,
  AViewSize: Single);
begin
  if (FContentSize <> AContentSize) or (FViewSize <> AViewSize) then
  begin
    FContentSize := AContentSize;
    FViewSize := AViewSize;
    UpdatePosition;
    UpdateVisibility;
  end;

end;

function TCastleScrollBar.PropertySections(const PropertyName: String
  ): TPropertySections;
begin
  if ArrayContainsString(PropertyName, ['ViewSize', 'ContentSize', 'Position',
    'ScrollBarType', 'Frame', 'Slider']) Then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleScrollBar.MouseDown(const APos: TVector2
  ): Boolean;
var
  R: TFloatRectangle;
  AScrollPos: TVector2;
begin
  R := SliderRect;
  if R.Contains(APos) then
  begin
    FDragOffset := GetPos(APos);
    if FScrollBarType = sbHorizontal then
      FDragOffset := FDragOffset - R.Left
    else
      FDragOffset := FDragOffset - R.Top;
    FDragging := True;
  end
  else
  begin
    if FScrollBarType = sbVertical then
      AScrollPos := APos + R.Size / 2
    else
      AScrollPos := APos - R.Size / 2;
    ScrollTo(MousePosToPosition(AScrollPos));
  end;
  Result := True;
end;
function TCastleScrollBar.MouseUp(const APos: TVector2): Boolean;
begin
  if FDragging then
  begin
    FDragging := false;
    Result := True;
  end else Result := False;
end;

function TCastleScrollBar.MouseMove(const APos: TVector2): Boolean;
var
  NewPos: single;
begin
  if FDragging then
  begin
    NewPos := PosValueToPosition(GetPos(APos) - FDragOffset) ;
    Position := NewPos;
    Result := True;
  end else Result := False;
end;

function TCastleScrollBar.SliderRect: TFloatRectangle;
var
  Pos:single;
begin
  Pos := FPosition * (GetSize - SliderSize) / (FContentSize - FViewSize);
  case FScrollBarType of
    sbHorizontal:
      Result := FloatRectangle(Pos, 0, SliderSize, EffectiveHeight);
    sbVertical:
      Result := FloatRectangle(0, EffectiveHeight - Pos - SliderSize, EffectiveWidth, SliderSize);
  end;
end;

function TCastleScrollBar.SliderSize: Single;
begin
  Result := FViewSize * GetSize / FContentSize;
  if Result < FSliderMinSize then Result := FSliderMinSize;
end;
function TCastleScrollBar.GetSize: single;
begin
  Result := 0;
  case FScrollBarType of
    sbHorizontal:
      Result := EffectiveWidth;
    sbVertical:
      Result := EffectiveHeight;
  end;
end;

function TCastleScrollBar.GetPos(const APos: TVector2): single;
begin
  Result := 0;
  case FScrollBarType of
    sbHorizontal:
      Result := APos.X;
    sbVertical:
      Result := APos.Y;
  end;
end;

procedure TCastleScrollBar.Scroll(const AValue: Single);
begin
  ScrollTo(FPosition + AValue);
end;
procedure TCastleScrollBar.ScrollTo(AValue: Single);
begin
  if FPosition <> AValue then
  begin
    AValue := Clamped(AValue, 0, FContentSize - FViewSize);
    if FPosition <> AValue then
    begin
      FPosition := AValue;
      if Assigned(FOnScroll) then
        FOnScroll(Self);
    end;
  end;
end;
procedure TCastleScrollBar.ScrollBy(const AValue: Single);
begin
  ScrollTo(FPosition + AValue);
end;

function TCastleScrollBar.PosValueToPosition(AValue: Single): Single;
begin
  if FScrollBarType = sbVertical then AValue := EffectiveHeight - AValue;
  Result := AValue * (FContentSize-FViewSize)/(GetSize-SliderSize);
  Result := Clamped(Result, 0, FContentSize - FViewSize);
end;

function TCastleScrollBar.MousePosToPosition(const APos: TVector2): Single;
begin
  Result := PosValueToPosition(GetPos(APos));
end;

procedure TCastleScrollBar.UpdateVisibility;
var
  NewExists: Boolean;
begin
  NewExists := (FViewSize < FContentSize);
  if NewExists <> Exists then
  begin
    if not NewExists then
      Position := 0;
    Exists := NewExists;
  end;
end;


{$endif read_implementation}
