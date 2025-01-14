unit CastleInternalPropertySelectDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
  ExtCtrls, CastleControls, CastleUIControls, CastleTransform, CastleViewport,
  RttiUtils, Generics.Collections;

type
  TPropertySelectResult = class
  public
    SelectedObject: TPersistent;
    SelectedComponent: TComponent;
    SelectedProperty: string;
    FriendlyObjectName: string;

    constructor Create;
    procedure Clear;
  end;

  TPropertySelectResultList = class(
 {$Ifdef fpc}specialize{$endif}TObjectList<TPropertySelectResult>)
  public
    function HasValidResult: boolean;
  end;

  TPropertySelectMode = (psmProperty, psmComponent);

  TPropertySelectForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Splitter1: TSplitter;
    TreeViewControls: TTreeView;
    TreeViewProperties: TTreeView;
    ButtonPanel: TButtonPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewControlsSelectionChanged(Sender: TObject);
    procedure TreeViewPropertiesDblClick(Sender: TObject);
  private
    FSelectResult: TPropertySelectResultList;
    FDblClickCompleted: boolean;
    FMode: TPropertySelectMode;
    procedure SetMode(const AValue: TPropertySelectMode);
    procedure UpdateSelectedResult;
  public
    property SelectResult: TPropertySelectResultList read FSelectResult;
    property Mode: TPropertySelectMode read FMode write SetMode;
    procedure Load(const AControl: TCastleUserInterface;
      const AsRoot: boolean = True; const AMode: TPropertySelectMode = psmProperty);
  end;

var
  Form1: TPropertySelectForm;

implementation

uses TypInfo, CastleStringUtils;

{$R *.lfm}

constructor TPropertySelectResult.Create;
begin
  inherited;

end;

procedure TPropertySelectResult.Clear;
begin
  SelectedObject := nil;
  SelectedComponent := nil;
  SelectedProperty := '';
  FriendlyObjectName := '';
end;

function TPropertySelectResultList.HasValidResult: boolean;
var
  R: TPropertySelectResult;
begin
  Result := False;
  for R in self do
  begin
    if Assigned(R.SelectedObject) and (R.SelectedProperty <> '') then Exit(True);
  end;
end;

procedure TPropertySelectForm.TreeViewControlsSelectionChanged(Sender: TObject);

  procedure FillTreeView(Persistent: TPersistent; TreeNode: TTreeNode;
    TreeView: TTreeView);
  var
    PropList: TPropInfoList;
    i: integer;
    PropValue: TObject;
    SubNode: TTreeNode;
  begin
    PropList := TPropInfoList.Create(Persistent, tkProperties);
    try
      for i := 0 to PropList.Count - 1 do
      begin
        if PropList[i]^.PropType^.Kind = tkClass then
        begin
          PropValue := GetObjectProp(Persistent, PropList[i]);
          if PropValue is TPersistent then
          begin
            if Assigned(TreeNode) then
              SubNode := TreeNode.Owner.AddChildObject(TreeNode,
                PropList[i]^.Name, PropValue)
            else
              SubNode := TreeView.Items.AddObject(nil,
                PropList[i]^.Name, PropValue);

            FillTreeView(TPersistent(PropValue), SubNode, TreeView);
          end;
        end
        else
        begin
          //TreeNode.Owner.AddChild(TreeNode, PropList[i]^.Name +
          //  ' = ' + GetPropValue(Persistent, PropList[i]));
          if Assigned(TreeNode) then
            TreeNode.Owner.AddChild(TreeNode, PropList[i]^.Name)
          else
            TreeView.Items.Add(nil, PropList[i]^.Name);
        end;
      end;
    finally
      PropList.Free;
    end;
  end;

var
  Node: TTreeNode;
begin
  if FMode <> psmProperty then Exit;

  TreeViewProperties.Items.BeginUpdate;
  try
    TreeViewProperties.Items.Clear;
    Node := TreeViewControls.Selected;
    if not Assigned(Node) then Exit;
    if not Assigned(Node.Data) then Exit;

    FillTreeView(TPersistent(Node.Data), nil, TreeViewProperties);
  finally
    TreeViewProperties.Items.EndUpdate;
  end;
end;

procedure TPropertySelectForm.TreeViewPropertiesDblClick(Sender: TObject);
begin
  UpdateSelectedResult;
  if FSelectResult.HasValidResult then
  begin
    FDblClickCompleted := True;
    ModalResult := mrOk;
  end;
end;

procedure TPropertySelectForm.UpdateSelectedResult;
var
  Node: TTreeNode;
  ComponentNode, PropertyNode: TTreeNode;
  R: TPropertySelectResult;
  I: integer;
begin
  FSelectResult.Clear;
  ComponentNode := TreeViewControls.Selected;

  if Fmode <> psmProperty then
  begin
    R := TPropertySelectResult.Create;
    FSelectResult.Add(R);

    if Assigned(ComponentNode) then
    begin
      R.SelectedComponent := TComponent(ComponentNode.Data);
      R.SelectedObject := TPersistent(ComponentNode.Data);
      R.FriendlyObjectName := ComponentNode.Text;
    end;
    Exit;
  end;

  for I := 0 to TreeViewProperties.SelectionCount - 1 do
  begin
    PropertyNode := TreeViewProperties.Selections[I];

    R := TPropertySelectResult.Create;
    FSelectResult.Add(R);

    if Assigned(ComponentNode) then
    begin
      R.SelectedComponent := TComponent(ComponentNode.Data);
      R.SelectedObject := TPersistent(ComponentNode.Data);
      R.FriendlyObjectName := ComponentNode.Text;
    end;

    if Assigned(PropertyNode) then
    begin
      if Assigned(PropertyNode.Data) then
      begin
        //child object, update FSelectedObject
        R.SelectedObject := TPersistent(PropertyNode.Data);
      end
      else
      begin
        //child object, update FSelectedObject
        if Assigned(PropertyNode.Parent) then
          R.SelectedObject :=
            TPersistent(PropertyNode.Parent.Data);
        R.SelectedProperty := PropertyNode.Text;
      end;
    end;


    R.FriendlyObjectName := '';
    if Assigned(ComponentNode) then
    begin
      Node := PropertyNode;
      if Assigned(Node) then
      begin
        if not Assigned(Node.Data) then  Node := Node.Parent;
        if Assigned(Node) then
        begin
          R.FriendlyObjectName := Node.Text;
          while True do
          begin
            Node := Node.Parent;
            if not Assigned(Node) then Break;
            R.FriendlyObjectName :=
              Node.Text + '.' + R.FriendlyObjectName;
          end;
        end;
      end;

      if R.FriendlyObjectName = '' then
        R.FriendlyObjectName := ComponentNode.Text
      else
        R.FriendlyObjectName :=
          ComponentNode.Text + '.' + R.FriendlyObjectName;
    end;
  end;

end;

procedure TPropertySelectForm.SetMode(const AValue: TPropertySelectMode);
begin
  if FMode = AValue then Exit;

  FMode := AValue;
  TreeViewProperties.Visible := FMode = psmProperty;
  if FMode = psmProperty then
  begin
    TreeViewProperties.Align := alClient;
    TreeViewControls.Align := alLeft;
    Splitter1.Visible := True;
    Caption := 'Select Property';
  end
  else
  begin
    TreeViewControls.Align := alClient;
    Splitter1.Visible := False;
    Caption := 'Select Component';
  end;
end;

procedure TPropertySelectForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not FDblClickCompleted then
    UpdateSelectedResult;
end;

procedure TPropertySelectForm.FormCreate(Sender: TObject);
begin
  FSelectResult := TPropertySelectResultList.Create(True);
  Mode := psmProperty;
end;

procedure TPropertySelectForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSelectResult);
end;

procedure TPropertySelectForm.Load(const AControl: TCastleUserInterface;
  const AsRoot: boolean; const AMode: TPropertySelectMode);

  procedure TraverseTransforms(ATransform: TCastleTransform; Node: TTreeNode);
  var
    T: TCastleTransform;
    C: TCastleBehavior;
    ChildNode: TTreeNode;
  begin
    for C in ATransform.BehaviorsEnumerate do
    begin
      if C.Name = '' then Continue;
      ChildNode := Node.Owner.AddChildObject(Node, C.Name, C);
    end;
    for T in ATransform do
    begin
      if T.Name = '' then Continue;
      ChildNode := Node.Owner.AddChildObject(Node, T.Name, T);
      TraverseTransforms(T, ChildNode);
    end;
  end;

  procedure TraverseUIControls(Control: TCastleUserInterface; Node: TTreeNode);
  var
    Child: TCastleUserInterface;
    ChildNode: TTreeNode;
    I: integer;
  begin
    for i := 0 to Control.ControlsCount - 1 do
    begin
      Child := Control.Controls[I];
      if Child.Name = '' then Continue;
      ChildNode := Node.Owner.AddChildObject(Node, Child.Name, Child);
      TraverseUIControls(Child, ChildNode);
    end;

    if Control is TCastleViewport then
    begin
      ChildNode := Node.Owner.AddChildObject(Node,
        (Control as TCastleViewport).Items.Name, (Control as TCastleViewport).Items);
      TraverseTransforms((Control as TCastleViewport).Items, ChildNode);
    end;

  end;

  procedure PopulateTreeView(UI: TCastleUserInterface; TreeView: TTreeView);
  var
    RootNode: TTreeNode;
  begin
    TreeView.Items.Clear;
    RootNode := TreeView.Items.Add(nil, UI.Name);
    TraverseUIControls(UI, RootNode);
    RootNode.Expand(True);
  end;

var
  ARoot: TCastleUserInterface;
begin
  FSelectResult.Clear;
  Mode := AMode;

  ARoot := AControl;
  if not AsRoot then
    while Assigned(ARoot.Parent) do ARoot := ARoot.Parent;
  PopulateTreeView(ARoot, TreeViewControls);
end;

end.
