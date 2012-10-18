package javax.swing.tree;
class VariableHeightLayoutCache {
  int maximalWidth;
  int totalHeight;
  int dirty;
  int row2node;
  int nodes;
  int expanded;
  class NodeRecord {
    int path;
    int bounds;
    int isExpanded;
    int node;
    int parent;
    int depth;
    int row;
  }
  int RECT_CACHE;
}
class TreeSelectionModel {
  int DISCONTIGUOUS_TREE_SELECTION;
  int CONTIGUOUS_TREE_SELECTION;
  int SINGLE_TREE_SELECTION;
}
class TreePath {
  int parentPath;
  int path;
  int serialVersionUID;
}
class TreeNode {
}
class TreeModel {
}
class TreeCellRenderer {
}
class TreeCellEditor {
}
class RowMapper {
}
class MutableTreeNode {
}
class FixedHeightLayoutCache {
  int maximalWidth;
  int totalHeight;
  int dirty;
  int row2node;
  int nodes;
  int expanded;
  class NodeRecord {
    int path;
    int bounds;
    int isExpanded;
    int node;
    int parent;
    int depth;
    int row;
  }
}
class ExpandVetoException {
  int event;
}
class DefaultTreeSelectionModel {
  int tmpPaths;
  int selectedPaths;
  int leadRow;
  int leadIndex;
  int leadPath;
  int selectionMode;
  int listSelectionModel;
  int rowMapper;
  int listenerList;
  int selection;
  int changeSupport;
  int SELECTION_MODE_PROPERTY;
  int serialVersionUID;
  class PathPlaceHolder {
    int isNew;
    int path;
  }
}
class DefaultTreeModel {
  int asksAllowsChildren;
  int listenerList;
  int root;
  int serialVersionUID;
}
class DefaultTreeCellRenderer {
  int borderSelectionColor;
  int backgroundNonSelectionColor;
  int backgroundSelectionColor;
  int textNonSelectionColor;
  int textSelectionColor;
  int openIcon;
  int leafIcon;
  int closedIcon;
  int drawsFocusBorderAroundIcon;
  int hasFocus;
  int selected;
}
class DefaultTreeCellEditor {
  int font;
  int editingIcon;
  int borderSelectionColor;
  int lastRow;
  int timer;
  int lastPath;
  int tree;
  int offset;
  int canEdit;
  int editingComponent;
  int editingContainer;
  int renderer;
  int realEditor;
  int listenerList;
  class DefaultTextField {
    int border;
    int serialVersionUID;
  }
  class EditorContainer {
    int serialVersionUID;
  }
}
class DefaultMutableTreeNode {
  class PostorderEnumeration {
    int childrenEnums;
    int nodes;
  }
  class PreorderEnumeration {
    int childrenEnums;
    int next;
  }
  class BreadthFirstEnumeration {
    int queue;
  }
  int allowsChildren;
  int userObject;
  int children;
  int parent;
  int EMPTY_ENUMERATION;
  int serialVersionUID;
}
class AbstractLayoutCache {
  int rowHeight;
  int rootVisible;
  int treeSelectionModel;
  int treeModel;
  int nodeDimensions;
  class NodeDimensions {
  }
}
