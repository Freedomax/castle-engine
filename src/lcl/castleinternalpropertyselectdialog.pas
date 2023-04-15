unit castleinternalpropertyselectdialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
  ExtCtrls, CastleControls, CastleUIControls, CastleTransform, CastleViewport,
  RttiUtils;

type
  TPropertySelectResult = record
    SelectedObject: TPersistent;
    SelectedProperty: string;
    FriendlyObjectName: string;
  end;

  TPropertySelectForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Splitter1: TSplitter;
    TreeViewControls: TTreeView;
    TreeViewProperties: TTreeView;
    ButtonPanel: TButtonPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure TreeViewControlsSelectionChanged(Sender: TObject);
  private
    FSelectResult: TPropertySelectResult;
    procedure UpdateSelectedResult;
  public
    property SelectResult: TPropertySelectResult read FSelectResult;
    procedure Load(const AControl: TCastleUserInterface);
  end;

var
  Form1: TPropertySelectForm;

implementation

uses TypInfo, CastleStringUtils;

{$R *.lfm}

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

procedure TPropertySelectForm.UpdateSelectedResult;
var
  Node: TTreeNode;
begin
  if Assigned(TreeViewControls.Selected) then
    FSelectResult.SelectedObject := TPersistent(TreeViewControls.Selected.Data)
  else
    FSelectResult.SelectedObject := nil;

  if Assigned(TreeViewProperties.Selected) then
  begin
    if Assigned(TreeViewProperties.Selected.Data) then
    begin
      //child object, update FSelectedObject
      FSelectResult.SelectedObject := TPersistent(TreeViewProperties.Selected.Data);
      FSelectResult.SelectedProperty := '';
    end
    else
    begin
      //child object, update FSelectedObject
      if Assigned(TreeViewProperties.Selected.Parent) then
        FSelectResult.SelectedObject :=
          TPersistent(TreeViewProperties.Selected.Parent.Data);
      FSelectResult.SelectedProperty := TreeViewProperties.Selected.Text;
    end;
  end
  else
    FSelectResult.SelectedProperty := '';

  FSelectResult.FriendlyObjectName := '';
  if Assigned(TreeViewControls.Selected) then
  begin
    Node := TreeViewProperties.Selected;
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
      FSelectResult.FriendlyObjectName := TreeViewControls.Selected.Text
    else
      FSelectResult.FriendlyObjectName :=
        TreeViewControls.Selected.Text + '.' +
        SuffixRemove('Persistent', FSelectResult.FriendlyObjectName, True);
  end;

end;

procedure TPropertySelectForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  UpdateSelectedResult;
end;

procedure TPropertySelectForm.Load(const AControl: TCastleUserInterface);

  procedure TraverseTransforms(ATransform: TCastleTransform; Node: TTreeNode);
  var
    T: TCastleTransform;
    ChildNode: TTreeNode;
  begin
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

begin
  PopulateTreeView(AControl, TreeViewControls);
end;

end.
