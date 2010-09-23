(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

type accel = {
  action : string;
  mask : Gdk.Tags.modifier list;
  value : Gdk.keysym;
}
open GdkKeysyms
let list = 
  [{action="Cut"; mask=[`CONTROL]; value= _X };
   {action="Copy"; mask=[`CONTROL]; value= _C };
   {action="Paste"; mask=[`CONTROL]; value= _V };
   {action="Print"; mask=[`CONTROL]; value= _P };
   {action="New"; mask=[`CONTROL]; value= _N };
   {action="Open"; mask=[`CONTROL]; value= _O };
   {action="Print"; mask=[`CONTROL]; value= _P };
  ]

let cols = new GTree.column_list
let action = cols#add Gobject.Data.string
let mask = cols#add Gobject.Data.int
let value = cols#add Gobject.Data.uint

let accel_edited (store:GTree.list_store) model path ~accel_key ~accel_mods ~hardware_keycode = 
  let iter = model#get_iter path in
  ignore (store#set ~row:iter ~column:mask accel_mods);
  ignore (store#set ~row:iter ~column:value accel_key)

let setup_tree_view (store:GTree.list_store) (treeview:GTree.view) =
  let renderer = GTree.cell_renderer_text [] in
  let column = 
    GTree.view_column 
      ~title:"Buy"
      ~renderer:(renderer,["text",action]) ()
  in
  ignore (treeview#append_column column);
  let renderer = GTree.cell_renderer_accel [`ACCEL_MODE `GTK; `EDITABLE true] in
  let column = 
    GTree.view_column 
      ~title:"Buy" 
      ~renderer:(renderer,[]) ()
  in
  column#add_attribute renderer "accel-mods" mask;
  column#add_attribute renderer "accel-key" value;
  ignore (treeview#append_column column);
  renderer#connect#accel_edited ~callback:(accel_edited store treeview#model)
    

let () = 
  let window = 
    GWindow.window ~kind:`TOPLEVEL ~title:"Accelerator Keys"
      ~border_width:10
      ~width:250
      ~height:250
      () 
  in
  let store = GTree.list_store cols in
  List.iter 
    (fun {action=a; mask=m; value=v} ->
       let row = store#append () in
	 store#set ~row ~column:action a;
	 store#set ~row ~column:mask 
	   (Gpointer.encode_flags GdkEnums.modifier m);
	 store#set ~row ~column:value v;
    )
    list;

  let scrolled_win = 
    GBin.scrolled_window ~packing:window#add ~hpolicy:`AUTOMATIC 
      ~vpolicy:`AUTOMATIC () 
  in 
  let treeview = GTree.view ~model:store ~packing:scrolled_win#add () in
  setup_tree_view store treeview;
  window#show ();
  GMain.main ()


    



(*
This code is an OCaml adaptation of the following C code from
http://www.linuxquestions.org/linux/articles/Technical/New_GTK_Widgets_GtkCellRendererAccel

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

enum
{
  ACTION = 0,
  MASK,
  VALUE,
  COLUMNS
};

typedef struct
{
  gchar *action;
  GdkModifierType mask;
  guint value;
} Accelerator;

const Accelerator list[] =
{
  {"Cut", GDK_CONTROL_MASK, GDK_X },
  { "Copy", GDK_CONTROL_MASK, GDK_C },
  { "Paste", GDK_CONTROL_MASK, GDK_V },
  { "New", GDK_CONTROL_MASK, GDK_N },
  { "Open", GDK_CONTROL_MASK, GDK_O },
  { "Print", GDK_CONTROL_MASK, GDK_P },
  { NULL, NULL, NULL }
};

static void setup_tree_view (GtkWidget* );
static void accel_edited (GtkCellRendererAccel*, gchar*, guint,
                          GdkModifierType, guint, GtkTreeView* );

int main (int argc,
          char *argv[])
{
  GtkWidget *window, *treeview, *scrolled_win;
  GtkListStore *store;
  GtkTreeIter iter;
  guint i = 0;

  gtk_init (&argc, &argv);

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), "Accelerator Keys");
  gtk_container_set_border_width (GTK_CONTAINER (window), 10);
  gtk_widget_set_size_request (window, 250, 250);

  treeview = gtk_tree_view_new ();
  setup_tree_view (treeview);
  
  store = gtk_list_store_new (COLUMNS, G_TYPE_STRING, G_TYPE_INT, G_TYPE_UINT);
  /* Add all of the keyboard accelerators to the GtkListStore. */
  while (list[i].action != NULL)
  {
    gtk_list_store_append (store, &iter);
    gtk_list_store_set (store, &iter, ACTION, list[i].action,
                        MASK, (gint) list[i].mask, VALUE, list[i].value, -1);
    i++;
  }

  gtk_tree_view_set_model (GTK_TREE_VIEW (treeview), GTK_TREE_MODEL (store));
  g_object_unref (store);

  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
                                  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add (GTK_CONTAINER (scrolled_win), treeview);
  gtk_container_add (GTK_CONTAINER (window), scrolled_win);
  gtk_widget_show_all (window);
  gtk_main ();
  return 0;
}

/* Create a tree view with two columns. The first is an action and the
 * second is a keyboard accelerator. */
static void
setup_tree_view (GtkWidget *treeview)
{
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes
                          ("Buy", renderer, "text", ACTION, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  renderer = gtk_cell_renderer_accel_new ();
  g_object_set (renderer, "accel-mode", GTK_CELL_RENDERER_ACCEL_MODE_GTK,
                   "editable", TRUE, NULL);
  column = gtk_tree_view_column_new_with_attributes ("Buy", renderer,
                                       "accel-mods", MASK, "accel-key", VALUE, NULL);

  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  g_signal_connect (G_OBJECT (renderer), "accel_edited",
                    G_CALLBACK (accel_edited),
                    (gpointer) treeview);
}

/* Apply the new keyboard accelerator key and mask to the cell. */
static void
accel_edited (GtkCellRendererAccel *renderer,
              gchar *path,
              guint accel_key,
              GdkModifierType mask,
              guint hardware_keycode,
              GtkTreeView *treeview)
{
  GtkTreeModel *model;
  GtkTreeIter iter;

  model = gtk_tree_view_get_model (treeview);
  if (gtk_tree_model_get_iter_from_string (model, &iter, path))
    gtk_list_store_set (GTK_LIST_STORE (model), &iter,
                        MASK, (gint) mask, VALUE, accel_key, -1);
  }
*)
