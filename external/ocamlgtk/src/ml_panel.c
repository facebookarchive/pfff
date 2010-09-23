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

#include <string.h>

#include <libgnomeui/gnome-client.h>
#include <libgnomeui/gnome-ui-init.h>

#include <panel-applet.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include "wrappers.h"
#include "ml_gobject.h"
#include "ml_gtk.h"
#include "ml_gdk.h"

#include "panel_tags.h"
#include "panel_tags.c"

#include "gtk_tags.h"

#define Panel_applet_val(v)  check_cast(PANEL_APPLET,v)

#define Val_orient_type Val_arrow_type
ML_1(panel_applet_get_orient, Panel_applet_val, Val_orient_type)
ML_1(panel_applet_get_size, Panel_applet_val, Val_int)

CAMLprim value ml_panel_applet_get_background(value app)
{
  GdkColor c;
  GdkPixmap *pixmap;
  PanelAppletBackgroundType typ;
  CAMLparam0();
  CAMLlocal3(v, d, tag);

  typ = panel_applet_get_background(Panel_applet_val(app), &c, &pixmap);
  tag = Val_background_type(typ);
  switch(typ) {
  case PANEL_NO_BACKGROUND:
    v = tag;
    break;
  case PANEL_COLOR_BACKGROUND:
    d = Val_copy(c);
    v = alloc_small(2, 0);
    Field(v, 0) = tag;
    Field(v, 1) = d;
    break;
  case PANEL_PIXMAP_BACKGROUND:
    d = Val_GdkPixmap(pixmap);
    v = alloc_small(2, 0);
    Field(v, 0) = tag;
    Field(v, 1) = d;
    break;
  }
  CAMLreturn(v);
}

#define Val_Panel_flags(f) ml_lookup_flags_getter(ml_table_panel_flags,f)
ML_1 (panel_applet_get_flags, Panel_applet_val, Val_Panel_flags)

Make_Flags_val(Panel_flags_val)
ML_2(panel_applet_set_flags, Panel_applet_val, Flags_Panel_flags_val, Unit)


static void ml_bonoboui_verb_fn(BonoboUIComponent *component, 
				gpointer user_data,
				const char *cname)
{
  value *clos = user_data;
  value verb;

  verb = copy_string(cname);
  callback_exn(*clos, verb);
}

static inline unsigned int list_length(value l)
{
  unsigned int len = 0;
  while(l != Val_emptylist) {
    len++;
    l = Field(l, 1);
  }
  return len;
}

static BonoboUIVerb* bonoboui_verbs_of_value(value verbs)
{
  unsigned int i, len = list_length(verbs);
  BonoboUIVerb *bonob_verbs;
  bonob_verbs = stat_alloc((len + 1) * sizeof (BonoboUIVerb));

  /* the global roots for the menu are leaked. 
     libpanelapplet does not provide hooks
     to be notified of closures destruction */
  for(i=0; i< len; i++) {
    value ml_verb = Field(verbs, 0);
    bonob_verbs[i].cname = String_val(Field(ml_verb, 0));
    bonob_verbs[i].cb = ml_bonoboui_verb_fn;
    bonob_verbs[i].user_data = ml_global_root_new(Field(ml_verb, 1));
    verbs = Field(verbs, 1);
  }
  memset(bonob_verbs + len, 0, sizeof (BonoboUIVerb));
  return bonob_verbs;
}

CAMLprim value ml_panel_applet_setup_menu(value app, value xml, value verbs)
{
  BonoboUIVerb *bonob_verbs = bonoboui_verbs_of_value(verbs);

  panel_applet_setup_menu(Panel_applet_val(app), String_val(xml), bonob_verbs, NULL);
  stat_free(bonob_verbs);
  return Val_unit;
}

CAMLprim value ml_panel_applet_setup_menu_from_file(value app, value opt_dir,
						    value file, value opt_appname,
						    value verbs)
{
  BonoboUIVerb *bonob_verbs = bonoboui_verbs_of_value(verbs);
  panel_applet_setup_menu_from_file(Panel_applet_val(app), String_option_val(opt_dir),
				    String_val(file), String_option_val(opt_appname),
				    bonob_verbs, NULL);
  stat_free(bonob_verbs);
  return Val_unit;
}


static void weak_notify(gpointer data, GObject *applet)
{
  value *glob_root = data;
  Field (*glob_root, 1) = 0;
  ml_global_root_destroy(glob_root);
}

static gboolean ml_panel_applet_factory_callback(PanelApplet *applet,
						 const gchar *iid,
						 gpointer user_data)
{
  value *ml_obj, *ml_factory = user_data;
  CAMLparam0();
  CAMLlocal3(ml_app, ml_iid, ret);

  ml_app = Val_pointer(applet);
  ml_obj = ml_global_root_new(ml_app);
  g_object_weak_ref(G_OBJECT(applet), weak_notify, ml_obj);
  ml_iid = copy_string(iid);
  ret = callback2_exn(*ml_factory, ml_app, ml_iid);
  if(Is_exception_result(ret) || ! Bool_val(ret))
    CAMLreturn(FALSE);
  else
    CAMLreturn(TRUE);
}


CAMLprim value ml_panel_applet_factory_main(value arg_arr, 
					    value iid, 
					    value ml_factory_cb)
{
  CAMLparam1(ml_factory_cb);
  int i, res;
  int argc = Wosize_val(arg_arr);
  char *prog_name, *argv[ argc ];

  for(i=0; i<argc; i++)
    argv[i] = String_val(Field(arg_arr, i));

  prog_name = g_path_get_basename(argv[0]);
  gnome_program_init (prog_name, NULL,
		      LIBGNOMEUI_MODULE,
		      argc, argv,
		      GNOME_CLIENT_PARAM_SM_CONNECT, FALSE,
		      GNOME_PARAM_NONE,
		      NULL);
  g_free(prog_name);
  res = panel_applet_factory_main(String_val(iid),
				  PANEL_TYPE_APPLET,
				  ml_panel_applet_factory_callback,
				  &ml_factory_cb);
  CAMLreturn(Val_not(Val_bool(res)));
}
