unit verse_view_intf;

{$mode objfpc}{$H+}

interface

uses
  Types, verse_visual_document;

type
  TRectArray = array of TRect;

  TVerseVisualChange = (
    vvcContent,
    vvcAssociations,
    vvcSelection,
    vvcHover,
    vvcStrongHighlight,
    vvcStyle,
    vvcBaseDirection,
    vvcTags,
    vvcViewport
  );
  TVerseVisualChanges = set of TVerseVisualChange;

  TVerseHit = record
    Revision: QWord;
    LogicalIndex: Integer;
    UTF16Offset: Integer;
    Trailing: Boolean;
  end;

  IVerseView = interface
    ['{7B734D05-FE0F-4056-B6DC-115CFECAFE41}']
    procedure SetDocument(const ADocument: IVerseVisualDocument);
    procedure NotifyChanges(Changes: TVerseVisualChanges; Revision: QWord);
    function HitTest(const ClientPoint: TPoint; out Hit: TVerseHit): Boolean;
    function GetLogicalRects(LogicalIndex: Integer;
      out Rects: TRectArray): Boolean;
    function TryGetLogicalScreenAnchor(LogicalIndex: Integer;
      out Anchor: TPoint): Boolean;
    function GetDisplayTags: Boolean;
    procedure SetDisplayTags(Value: Boolean);
    property DisplayTags: Boolean read GetDisplayTags write SetDisplayTags;
  end;

implementation

end.
