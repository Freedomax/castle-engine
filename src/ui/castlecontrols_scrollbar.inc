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
    FType: TCastleScrollBarType;
    FViewSize, FContentSize: Single;
    FPosition: Single;
    FDragOffset :single;
    FOnScroll: TNotifyEvent;
    FThumbColor: TCastleColor;
    FThumbAlpha: single;
    FThumbMinSize, FThumbMargin: Single;
    FHidden: Boolean;
    FDragging: Boolean;
    procedure SetContentSize(const AValue: Single);
    procedure SetViewSize(const AValue: Single);
    procedure UpdatePosition;
  protected
    procedure UIScaleChanged; override;
    function ThumbRect: TFloatRectangle;
    function ThumbSize: Single;
    procedure Scroll(const AValue: Single);
    procedure ScrollTo(AValue: Single);
    procedure ScrollBy(const AValue: Single);
    function MousePosToPosition(const APos: TVector2): Single;
    function PosValueToPosition(const AValue: Single): Single;
    procedure UpdateVisibility;
    function GetSize:single;
    function GetPos(const APos:TVector2):single;
    function MouseDown(const  APos: TVector2): Boolean;
    function MouseMove(const APos: TVector2): Boolean;
    function MouseUp(const  APos: TVector2): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Resize; override;
    procedure Render; override;
    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;


    procedure SetContentAndViewSize(const AContentSize, AViewSize: Single);

    property ViewSize: Single read FViewSize write SetViewSize ;
    property ContentSize: Single read FContentSize write SetContentSize ;
    property Position: Single read FPosition write ScrollTo;
    property ScrollBarType: TCastleScrollBarType read FType write FType;
    property ThumbColor: TCastleColor read FThumbColor write FThumbColor;
    property ThumbAlpha: Single read FThumbAlpha write FThumbAlpha;
    property ThumbMinSize: Single read FThumbMinSize write FThumbMinSize;
    property ThumbMargin: Single read FThumbMargin write FThumbMargin;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
  end;


{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  FViewSize := 1;
  FContentSize := 1;
  FPosition := 0;
  FType := sbHorizontal;
  FThumbColor := White;
  FThumbAlpha := 1;
  FThumbMinSize := 8;
  FThumbMargin := 2;
  FHidden := False;
  FDragging := False;
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

    procedure DrawThumb;
    var
      RThumb: TFloatRectangle;
    begin
      RThumb := ThumbRect;
      RThumb := FloatRectangle(RThumb.LeftBottom * UIScale, RThumb.Width * UIScale, RThumb.Height * UIScale);
      RThumb.LeftBottom := R.LeftBottom + RThumb.LeftBottom;
      Theme.Draw(RThumb, TThemeImage.tiScrollbarSlider, UIScale);
    end;

    procedure DrawScrollArea;
    begin
      Theme.Draw(R, TThemeImage.tiScrollbarFrame, UIScale);
    end;

begin
  inherited;
  R := RenderRect;
  DrawScrollArea;
  DrawThumb;
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

function TCastleScrollBar.MouseDown(const APos: TVector2
  ): Boolean;
var
  R: TFloatRectangle;
begin
  R := ThumbRect;
  if R.Contains(APos) then
  begin
    FDragOffset:= GetPos(APos);
    if FType = sbHorizontal then
      FDragOffset := FDragOffset - R.Left
    else
      FDragOffset := FDragOffset - R.Top;
    FDragging := True;
  end
  else
  begin
    ScrollTo(MousePosToPosition(APos - R.Size / 2));
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

function TCastleScrollBar.ThumbRect: TFloatRectangle;
var
  Pos:single;
begin
  Pos := FPosition * (GetSize - ThumbSize) / (FContentSize - FViewSize);
  case FType of
    sbHorizontal:
      Result := FloatRectangle(Pos, 0, ThumbSize, EffectiveHeight);
    sbVertical:
      Result := FloatRectangle(0,EffectiveHeight - Pos - ThumbSize, EffectiveWidth, ThumbSize);
  end;

end;

function TCastleScrollBar.ThumbSize: Single;
begin
  Result := FViewSize * GetSize / FContentSize;
  if Result < FThumbMinSize then Result := FThumbMinSize;
end;
function TCastleScrollBar.GetSize: single;
begin
  case FType of
    sbHorizontal:
      Result := EffectiveWidth;
    sbVertical:
      Result := EffectiveHeight;
  end;
end;

function TCastleScrollBar.GetPos(const APos: TVector2): single;
begin
  case FType of
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

function TCastleScrollBar.PosValueToPosition(const AValue: Single): Single;
begin
  Result := AValue * (FContentSize-FViewSize)/(GetSize-ThumbSize);
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
