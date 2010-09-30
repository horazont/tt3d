unit PathfinderBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

  { TPathfinderBase }

  generic TPathfinderBase<PNodeType, TQueue> = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;
  protected
    FClosedTable, FOpenTable: TFPDataHashTable;
    FClosedList, FOpenList: TFPList;
    FOpenQueue: TQueue;
  protected
    procedure Clear;
    function HashNode(ANode: PNodeType): String; virtual;
    function InClosedList(ANode: PNodeType): Boolean;
    function InOpenList(ANode: PNodeType): Boolean;
  end;

implementation

{ TPathfinderBase }

constructor TPathfinderBase.Create;
begin
  FClosedTable := TFPDataHashTable.Create;
  FClosedList := TFPList.Create;
  FOpenTable := TFPDataHashTable.Create;
  FOpenList := TFPList.Create;
  FOpenQueue := TQueue.Create;
end;

destructor TPathfinderBase.Destroy;
begin
  Clear;
  FOpenList.Free;
  FOpenQueue.Free;
  FOpenTable.Free;
  FClosedTable.Free;
  FClosedList.Free;
  inherited Destroy;
end;

procedure TPathfinderBase.Clear;
var
  I: Integer;
  Val: PNodeType;
begin
  for I := 0 to FOpenList.Count - 1 do
  begin
    Val := PNodeType(FOpenList[I]);
    FreeMem(Val);
  end;
  FOpenList.Clear;
  FOpenQueue.Clear;
  FOpenTable.Clear;

  for I := 0 to FClosedList.Count - 1 do
    FreeMem(PNodeType(FClosedList[I]));
  FClosedList.Clear;
  FClosedTable.Clear;
end;

function TPathfinderBase.HashNode(ANode: PNodeType): String;
begin
  Result := 'You did not override HashNode';
end;

function TPathfinderBase.InClosedList(ANode: PNodeType): Boolean;
begin
  Result := FClosedTable.Items[HashNode(ANode)] <> nil;
end;

function TPathfinderBase.InOpenList(ANode: PNodeType): Boolean;
begin
  Result := FOpenTable.Items[HashNode(ANode)] <> nil
end;

end.

