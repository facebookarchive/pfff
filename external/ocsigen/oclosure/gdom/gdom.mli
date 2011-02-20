open Js

#include "domHelper.mli"
#include "tagName.mli"
#include "abstractRange.mli"

val setTextContent : #Dom_html.element t -> js_string t -> unit

module A11y : sig
  type role =
      BUTTON 
  (* ARIA role for a checkbox button element.*)
    | CHECKBOX 
  (* ARIA role for a combobox element.*)
    | COMBOBOX 
  (* ARIA role for a dialog element.*)
    | DIALOG 
  (* ARIA role for link.*)
    | LINK 
  (* ARIA role for listbox.*)
    | LISTBOX 
  (* ARIA role for popup menu, submenu elements etc.*)
    | MAIN 
  (* ARIA role for main content in a document.*)
    | MENU 
  (* ARIA role for a menubar element containing menu elements.*)
    | MENUBAR 
  (* ARIA role for menu item elements.*)
    | MENU_ITEM 
  (* ARIA role for a checkbox box element inside a menu.*)
    | MENU_ITEM_CHECKBOX 
  (* ARIA role for a radio button element inside a menu.*)
    | MENU_ITEM_RADIO 
  (* ARIA role for option items, generally used with a parent of listbox.*)
    | NAVIGATION 
  (* ARIA role for a collection of links suitable for use when navigating
     the document or related documents.*)
    | OPTION 
  (* ARIA role for a group of elements like a group of radio buttons,
     a form, etc.*)
    | GROUP 
  (* ARIA role for a slider.*)
    | SLIDER 
  (* ARIA role for a tab button.*)
    | TAB 
  (* ARIA role for a tab bar (i.e. a list of tab buttons).*)
    | TAB_LIST 
  (* ARIA role for a tab page (i.e. the element holding tab contents).*)
    | TAB_PANEL 
  (* ARIA role for a toolbar element.*)
    | TOOLBAR 

  type role_pre
	
  val role_pre_of_role : role -> role_pre
	  
  val role_of_role_pre : role_pre -> role

end
