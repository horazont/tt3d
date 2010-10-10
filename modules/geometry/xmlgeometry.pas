unit XMLGeometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Geometry, xml;

function LoadVector2(XMLNode: TxmlNode): TVector2;
function LoadVector2(XMLNode: TxmlNode; ADefault: TVector2): TVector2;
function LoadVector3(XMLNode: TxmlNode): TVector3;
function LoadVector3(XMLNode: TxmlNode; ADefault: TVector3): TVector3;
function LoadVector4(XMLNode: TxmlNode): TVector4;
function LoadVector4(XMLNode: TxmlNode; ADefault: TVector4): TVector4;

procedure SaveVector(XMLNode: TxmlNode; AVector: TVector2);
procedure SaveVector(XMLNode: TxmlNode; AVector: TVector3);
procedure SaveVector(XMLNode: TxmlNode; AVector: TVector4);

implementation

function LoadVector2(XMLNode: TxmlNode): TVector2;
begin
  Result.X := XMLNode.AttributeDouble['x'];
  Result.Y := XMLNode.AttributeDouble['y'];
end;

function LoadVector2(XMLNode: TxmlNode; ADefault: TVector2): TVector2;
begin
  if XMLNode = nil then
    Exit(ADefault)
  else
    Exit(LoadVector2(XMLNode));
end;

function LoadVector3(XMLNode: TxmlNode): TVector3;
begin
  Result.X := XMLNode.AttributeDouble['x'];
  Result.Y := XMLNode.AttributeDouble['y'];
  Result.Z := XMLNode.AttributeDouble['z'];
end;

function LoadVector3(XMLNode: TxmlNode; ADefault: TVector3): TVector3;
begin
  if XMLNode = nil then
    Exit(ADefault)
  else
    Exit(LoadVector3(XMLNode));
end;

function LoadVector4(XMLNode: TxmlNode): TVector4;
begin
  Result.X := XMLNode.AttributeDouble['x'];
  Result.Y := XMLNode.AttributeDouble['y'];
  Result.Z := XMLNode.AttributeDouble['z'];
  Result.W := XMLNode.AttributeDouble['w'];
end;

function LoadVector4(XMLNode: TxmlNode; ADefault: TVector4): TVector4;
begin
  if XMLNode = nil then
    Exit(ADefault)
  else
    Exit(LoadVector4(XMLNode));
end;

procedure SaveVector(XMLNode: TxmlNode; AVector: TVector2);
begin
  XMLNode.AttributeDouble['x'] := AVector.X;
  XMLNode.AttributeDouble['y'] := AVector.Y;
end;

procedure SaveVector(XMLNode: TxmlNode; AVector: TVector3);
begin
  XMLNode.AttributeDouble['x'] := AVector.X;
  XMLNode.AttributeDouble['y'] := AVector.Y;
  XMLNode.AttributeDouble['z'] := AVector.Z;
end;

procedure SaveVector(XMLNode: TxmlNode; AVector: TVector4);
begin
  XMLNode.AttributeDouble['x'] := AVector.X;
  XMLNode.AttributeDouble['y'] := AVector.Y;
  XMLNode.AttributeDouble['z'] := AVector.Z;
  XMLNode.AttributeDouble['w'] := AVector.W;
end;

end.

