/**************************************************************************/
/*                Lablgtk                                                 */
/*                                                                        */
/*    This program is free software; you can redistribute it              */
/*    and/or modify it under the terms of the GNU Library General         */
/*    Public License as published by the Free Software Foundation         */
/*    version 2, with the exception described in file COPYING which       */
/*    comes with the library.                                             */
/*                                                                        */
/*    This program is distributed in the hope that it will be useful,     */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of      */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       */
/*    GNU Library General Public License for more details.                */
/*                                                                        */
/*    You should have received a copy of the GNU Library General          */
/*    Public License along with this program; if not, write to the        */
/*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         */
/*    Boston, MA 02111-1307  USA                                          */
/*                                                                        */
/*                                                                        */
/**************************************************************************/

/* $Id: ml_gtknew.c 1419 2008-09-22 13:37:06Z zoggy $ */

#include <gtk/gtk.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "wrappers.h"
#include "ml_glib.h"
#include "ml_gdk.h"
#include "ml_gtk.h"

static value ml_class_init=0;

static void class_init (value class)
{
  callback_exn(ml_class_init, class);
}


CAMLprim value set_ml_class_init (value class_func)
{
  if (!ml_class_init) register_global_root (&ml_class_init);
  ml_class_init = class_func;
  return Val_unit;
}

CAMLprim value ml_gtk_type_new (value type)
{
  return Val_GtkWidget_sink(gtk_type_new(Int_val(type)));
}


struct widget_info {
  guint size;
  guint class_size;
  guint (*get_type_func)(void);
}

widget_info_array[] = {
  { sizeof(GtkObject), sizeof(GtkObjectClass), gtk_object_get_type },
  { sizeof(GtkWidget), sizeof(GtkWidgetClass), gtk_widget_get_type },
  { sizeof(GtkMisc), sizeof(GtkMiscClass), gtk_misc_get_type },
  { sizeof(GtkLabel), sizeof(GtkLabelClass), gtk_label_get_type },
  { sizeof(GtkAccelLabel), sizeof(GtkAccelLabelClass), gtk_accel_label_get_type },
  { sizeof(GtkTipsQuery), sizeof(GtkTipsQueryClass), gtk_tips_query_get_type },
  { sizeof(GtkArrow), sizeof(GtkArrowClass), gtk_arrow_get_type },
  { sizeof(GtkImage), sizeof(GtkImageClass), gtk_image_get_type },
  { sizeof(GtkPixmap), sizeof(GtkPixmapClass), gtk_pixmap_get_type },
  { sizeof(GtkContainer), sizeof(GtkContainerClass), gtk_container_get_type },
  { sizeof(GtkBin), sizeof(GtkBinClass), gtk_bin_get_type },
  { sizeof(GtkAlignment), sizeof(GtkAlignmentClass), gtk_alignment_get_type },
  { sizeof(GtkFrame), sizeof(GtkFrameClass), gtk_frame_get_type },
  { sizeof(GtkAspectFrame), sizeof(GtkAspectFrameClass), gtk_aspect_frame_get_type },
  { sizeof(GtkButton), sizeof(GtkButtonClass), gtk_button_get_type },
  { sizeof(GtkToggleButton), sizeof(GtkToggleButtonClass), gtk_toggle_button_get_type },
  { sizeof(GtkCheckButton), sizeof(GtkCheckButtonClass), gtk_check_button_get_type },
  { sizeof(GtkRadioButton), sizeof(GtkRadioButtonClass), gtk_radio_button_get_type },
  { sizeof(GtkOptionMenu), sizeof(GtkOptionMenuClass), gtk_option_menu_get_type },
  { sizeof(GtkItem), sizeof(GtkItemClass), gtk_item_get_type },
  { sizeof(GtkMenuItem), sizeof(GtkMenuItemClass), gtk_menu_item_get_type },
  { sizeof(GtkCheckMenuItem), sizeof(GtkCheckMenuItemClass), gtk_check_menu_item_get_type },
  { sizeof(GtkRadioMenuItem), sizeof(GtkRadioMenuItemClass), gtk_radio_menu_item_get_type },
  { sizeof(GtkTearoffMenuItem), sizeof(GtkTearoffMenuItemClass), gtk_tearoff_menu_item_get_type },
  { sizeof(GtkListItem), sizeof(GtkListItemClass), gtk_list_item_get_type },
  { sizeof(GtkTreeItem), sizeof(GtkTreeItemClass), gtk_tree_item_get_type },
  { sizeof(GtkWindow), sizeof(GtkWindowClass), gtk_window_get_type },
  { sizeof(GtkColorSelectionDialog), sizeof(GtkColorSelectionDialogClass), gtk_color_selection_dialog_get_type },
  { sizeof(GtkDialog), sizeof(GtkDialogClass), gtk_dialog_get_type },
  { sizeof(GtkInputDialog), sizeof(GtkInputDialogClass), gtk_input_dialog_get_type },
  { sizeof(GtkFileSelection), sizeof(GtkFileSelectionClass), gtk_file_selection_get_type },
  { sizeof(GtkFontSelectionDialog), sizeof(GtkFontSelectionDialogClass), gtk_font_selection_dialog_get_type },
  { sizeof(GtkPlug), sizeof(GtkPlugClass), gtk_plug_get_type },
  { sizeof(GtkEventBox), sizeof(GtkEventBoxClass), gtk_event_box_get_type },
  { sizeof(GtkHandleBox), sizeof(GtkHandleBoxClass), gtk_handle_box_get_type },
  { sizeof(GtkScrolledWindow), sizeof(GtkScrolledWindowClass), gtk_scrolled_window_get_type },
  { sizeof(GtkViewport), sizeof(GtkViewportClass), gtk_viewport_get_type },
  { sizeof(GtkBox), sizeof(GtkBoxClass), gtk_box_get_type },
  { sizeof(GtkButtonBox), sizeof(GtkButtonBoxClass), gtk_button_box_get_type },
  { sizeof(GtkHButtonBox), sizeof(GtkHButtonBoxClass), gtk_hbutton_box_get_type },
  { sizeof(GtkVButtonBox), sizeof(GtkVButtonBoxClass), gtk_vbutton_box_get_type },
  { sizeof(GtkVBox), sizeof(GtkVBoxClass), gtk_vbox_get_type },
  { sizeof(GtkColorSelection), sizeof(GtkColorSelectionClass), gtk_color_selection_get_type },
  { sizeof(GtkGammaCurve), sizeof(GtkGammaCurveClass), gtk_gamma_curve_get_type },
  { sizeof(GtkHBox), sizeof(GtkHBoxClass), gtk_hbox_get_type },
  { sizeof(GtkCombo), sizeof(GtkComboClass), gtk_combo_get_type },
  { sizeof(GtkStatusbar), sizeof(GtkStatusbarClass), gtk_statusbar_get_type },
  { sizeof(GtkStatusIcon), sizeof(GtkStatusIconClass), gtk_status_icon_get_type },
  { sizeof(GtkCList), sizeof(GtkCListClass), gtk_clist_get_type },
  { sizeof(GtkCTree), sizeof(GtkCTreeClass), gtk_ctree_get_type },
  { sizeof(GtkFixed), sizeof(GtkFixedClass), gtk_fixed_get_type },
  { sizeof(GtkNotebook), sizeof(GtkNotebookClass), gtk_notebook_get_type },
  { sizeof(GtkFontSelection), sizeof(GtkFontSelectionClass), gtk_font_selection_get_type },
  { sizeof(GtkPaned), sizeof(GtkPanedClass), gtk_paned_get_type },
  { sizeof(GtkHPaned), sizeof(GtkHPanedClass), gtk_hpaned_get_type },
  { sizeof(GtkVPaned), sizeof(GtkVPanedClass), gtk_vpaned_get_type },
  { sizeof(GtkLayout), sizeof(GtkLayoutClass), gtk_layout_get_type },
  { sizeof(GtkList), sizeof(GtkListClass), gtk_list_get_type },
  { sizeof(GtkMenuShell), sizeof(GtkMenuShellClass), gtk_menu_shell_get_type },
  { sizeof(GtkMenuBar), sizeof(GtkMenuBarClass), gtk_menu_bar_get_type },
  { sizeof(GtkMenu), sizeof(GtkMenuClass), gtk_menu_get_type },
  { sizeof(GtkPacker), sizeof(GtkPackerClass), gtk_packer_get_type },
  { sizeof(GtkSocket), sizeof(GtkSocketClass), gtk_socket_get_type },
  { sizeof(GtkTable), sizeof(GtkTableClass), gtk_table_get_type },
  { sizeof(GtkToolbar), sizeof(GtkToolbarClass), gtk_toolbar_get_type },
  { sizeof(GtkTree), sizeof(GtkTreeClass), gtk_tree_get_type },
  { sizeof(GtkCalendar), sizeof(GtkCalendarClass), gtk_calendar_get_type },
  { sizeof(GtkDrawingArea), sizeof(GtkDrawingAreaClass), gtk_drawing_area_get_type },
  { sizeof(GtkCurve), sizeof(GtkCurveClass), gtk_curve_get_type },
  { sizeof(GtkEditable), sizeof(GtkEditableClass), gtk_editable_get_type },
  { sizeof(GtkEntry), sizeof(GtkEntryClass), gtk_entry_get_type },
  { sizeof(GtkSpinButton), sizeof(GtkSpinButtonClass), gtk_spin_button_get_type },
  { sizeof(GtkText), sizeof(GtkTextClass), gtk_text_get_type },
  { sizeof(GtkRuler), sizeof(GtkRulerClass), gtk_ruler_get_type },
  { sizeof(GtkHRuler), sizeof(GtkHRulerClass), gtk_hruler_get_type },
  { sizeof(GtkVRuler), sizeof(GtkVRulerClass), gtk_vruler_get_type },
  { sizeof(GtkRange), sizeof(GtkRangeClass), gtk_range_get_type },
  { sizeof(GtkScale), sizeof(GtkScaleClass), gtk_scale_get_type },
  { sizeof(GtkHScale), sizeof(GtkHScaleClass), gtk_hscale_get_type },
  { sizeof(GtkVScale), sizeof(GtkVScaleClass), gtk_vscale_get_type },
  { sizeof(GtkScrollbar), sizeof(GtkScrollbarClass), gtk_scrollbar_get_type },
  { sizeof(GtkHScrollbar), sizeof(GtkHScrollbarClass), gtk_hscrollbar_get_type },
  { sizeof(GtkVScrollbar), sizeof(GtkVScrollbarClass), gtk_vscrollbar_get_type },
  { sizeof(GtkSeparator), sizeof(GtkSeparatorClass), gtk_separator_get_type },
  { sizeof(GtkHSeparator), sizeof(GtkHSeparatorClass), gtk_hseparator_get_type },
  { sizeof(GtkVSeparator), sizeof(GtkVSeparatorClass), gtk_vseparator_get_type },
  { sizeof(GtkProgress), sizeof(GtkProgressClass), gtk_progress_get_type },
  { sizeof(GtkProgressBar), sizeof(GtkProgressBarClass), gtk_progress_bar_get_type },
  { sizeof(GtkData), sizeof(GtkDataClass), gtk_data_get_type },
  { sizeof(GtkAdjustment), sizeof(GtkAdjustmentClass), gtk_adjustment_get_type },
  { sizeof(GtkTooltips), sizeof(GtkTooltipsClass), gtk_tooltips_get_type },
  { sizeof(GtkItemFactory), sizeof(GtkItemFactoryClass), gtk_item_factory_get_type }
};


CAMLprim value ml_gtk_type_unique (value name, value parent, value nsignals)
{
  struct widget_info * wi;
  GtkTypeInfo ttt_info;

  wi = widget_info_array + Int_val(parent);
  ttt_info.type_name = String_val(name);
  ttt_info.object_size = wi->size;
  ttt_info.class_size = wi->class_size + Int_val(nsignals)*sizeof(void *);
  ttt_info.class_init_func = (GtkClassInitFunc) class_init;
  ttt_info.object_init_func = (GtkObjectInitFunc) NULL;
  ttt_info.reserved_1 = NULL;
  ttt_info.reserved_2 = NULL;
  ttt_info.base_class_init_func = (GtkClassInitFunc) NULL;

  return Val_int(gtk_type_unique(wi->get_type_func (), &ttt_info));
}

static guint sig[100];

CAMLprim value ml_gtk_object_class_add_signals (value class, value signals,
                                                value nsignals)
{
  int i;
  for (i=0; i<nsignals; i++)
    sig[i] = Int_val(Field(signals, i));
  gtk_object_class_add_signals ((GtkObjectClass *)class,
	       sig, Int_val(nsignals));
  return Val_unit;
}

CAMLprim value ml_gtk_signal_new (value name, value run_type, value classe,
                                  value parent, value num)
{
  struct widget_info * wi;
  int offset;

  wi = widget_info_array + Int_val(parent);
  offset = wi->class_size+Int_val(num)*sizeof(void *);
  return Val_int(gtk_signal_new (String_val(name), Int_val(run_type),
		   ((GtkObjectClass *)classe)->type, offset,
		   gtk_signal_default_marshaller, GTK_TYPE_NONE, 0));
  *(((int *)classe)+offset) = 0;
}
