package javax.swing.table;
class TableModel {
}
class TableColumnModel {
}
class TableColumn {
  int changeSupport;
  int resizedPostingDisableCount;
  int isResizable;
  int cellEditor;
  int cellRenderer;
  int headerValue;
  int headerRenderer;
  int maxWidth;
  int preferredWidth;
  int minWidth;
  int width;
  int identifier;
  int modelIndex;
  int CELL_RENDERER_PROPERTY;
  int HEADER_RENDERER_PROPERTY;
  int HEADER_VALUE_PROPERTY;
  int COLUMN_WIDTH_PROPERTY;
  int serialVersionUID;
}
class TableCellRenderer {
}
class TableCellEditor {
}
class JTableHeader {
  int cellRenderer;
  int updateTableInRealTime;
  int table;
  int resizingColumn;
  int resizingAllowed;
  int reorderingAllowed;
  int opaque;
  int draggedDistance;
  int draggedColumn;
  int columnModel;
  int serialVersionUID;
  class AccessibleJTableHeader {
    class AccessibleJTableHeaderEntry {
      int table;
      int parent;
      int columnIndex;
    }
  }
}
class DefaultTableModel {
  int columnIdentifiers;
  int dataVector;
  int serialVersionUID;
}
class DefaultTableColumnModel {
  int totalColumnWidth;
  int columnSelectionAllowed;
  int changeEvent;
  int listenerList;
  int columnMargin;
  int selectionModel;
  int tableColumns;
  int serialVersionUID;
}
class DefaultTableCellRenderer {
  int background;
  int foreground;
  class UIResource {
  }
  int noFocusBorder;
  int serialVersionUID;
}
class AbstractTableModel {
  int listenerList;
  int serialVersionUID;
}
