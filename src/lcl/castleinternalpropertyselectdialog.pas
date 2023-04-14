unit castleinternalpropertyselectdialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, CastleControls, CastleUIControls, CastleTransform, CastleViewport, RttiUtils;

type
  TPropertySelectForm = class(TForm)
    TreeViewControls: TTreeView;
    TreeViewProperties: TTreeView;
    ButtonPanel: TButtonPanel;
    procedure TreeViewControlsSelectionChanged(Sender: TObject);
  private

  public
    procedure Load(const AControl: TCastleUserInterface);
  end;

var
  Form1: TPropertySelectForm;

implementation

uses TypInfo;

{$R *.lfm}

procedure TPropertySelectForm.TreeViewControlsSelectionChanged(Sender: TObject);

  procedure FillTreeView(Persistent: TPersistent; TreeNode: TTreeNode);
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

          SubNode := TreeNode.Owner.AddChildObject(TreeNode,
            PropList[i]^.Name, PropValue);
          if PropValue is TPersistent then
            FillTreeView(TPersistent(PropValue), SubNode);

        end
        else
        begin
          //TreeNode.Owner.AddChild(TreeNode, PropList[i]^.Name +
          //  ' = ' + GetPropValue(Persistent, PropList[i]));
          TreeNode.Owner.AddChild(TreeNode, PropList[i]^.Name);
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

    FillTreeView(TPersistent(Node.Data),
      TreeViewProperties.Items.Add(nil, 'MyPersistentObject'));
  finally
    TreeViewProperties.Items.EndUpdate;
  end;
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
