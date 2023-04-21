unit CastleInternalPropertySelectDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
  ExtCtrls, CastleControls, CastleUIControls, CastleTransform, CastleViewport,
  RttiUtils;

type
  TPropertySelectResult = class
  public
    SelectedObject: TPersistent;
    SelectedProperty: string;
    FriendlyObjectName: string;

    constructor Create;
    procedure Clear;
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
    FSelectResult: TPropertySelectResult;
    FDblClickCompleted: boolean;
    FMode: TPropertySelectMode;
    procedure SetMode(const AValue: TPropertySelectMode);
    procedure UpdateSelectedResult;
  public
    property SelectResult: TPropertySelectResult read FSelectResult;
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
  SelectedProperty := '';
  FriendlyObjectName := '';
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
  if Assigned(FSelectResult.SelectedObject) and
    (FSelectResult.SelectedProperty <> '') then
  begin
    FDblClickCompleted := True;
    ModalResult := mrOk;
  end;
end;

procedure TPropertySelectForm.UpdateSelectedResult;
var
  Node: TTreeNode;
  ComponentNode, PropertyNode: TTreeNode;
begin
  FSelectResult.Clear;

  ComponentNode := TreeViewControls.Selected;
  PropertyNode := TreeViewProperties.Selected;

  if Assigned(ComponentNode) then
  begin
    FSelectResult.SelectedObject := TPersistent(ComponentNode.Data);
    FSelectResult.FriendlyObjectName := ComponentNode.Text;
  end;

  if Fmode <> psmProperty then Exit;

  if Assigned(PropertyNode) then
  begin
    if Assigned(PropertyNode.Data) then
    begin
      //child object, update FSelectedObject
      FSelectResult.SelectedObject := TPersistent(TreeViewProperties.Selected.Data);
    end
    else
    begin
      //child object, update FSelectedObject
      if Assigned(PropertyNode.Parent) then
        FSelectResult.SelectedObject :=
          TPersistent(PropertyNode.Parent.Data);
      FSelectResult.SelectedProperty := PropertyNode.Text;
    end;
  end;


  FSelectResult.FriendlyObjectName := '';
  if Assigned(ComponentNode) then
  begin
    Node := PropertyNode;
    if Assigned(Node) then
    begin
      if not Assigned(Node.Data) then  Node := Node.Parent;
      if Assigned(Node) then
      begin
        FSelectResult.FriendlyObjectName := Node.Text;
        while True do
        begin
          Node := Node.Parent;
          if not Assigned(Node) then Break;
          FSelectResult.FriendlyObjectName :=
            Node.Text + '.' + FSelectResult.FriendlyObjectName;
        end;
      end;
    end;

    if FSelectResult.FriendlyObjectName = '' then
      FSelectResult.FriendlyObjectName := ComponentNode.Text
    else
      FSelectResult.FriendlyObjectName :=
        ComponentNode.Text + '.' + FSelectResult.FriendlyObjectName;
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
  FSelectResult := TPropertySelectResult.Create;
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
