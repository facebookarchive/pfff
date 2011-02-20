(*
   OClosure Project - 2010
   
   Class goog.i18n.CharPickerData

   @author : Oran Charles
   @version 0.2
*)

#ifndef I18N
open Js
#endif

class type charPickerData = object
(** Top catagory names of character organization.*)
  method categories : js_string t js_array t prop

(** 
   Character lists in base88 encoding scheme. Each subarray is a list of
   base88 encoded charater strings representing corresponding subcategory
   specified in [goog.i18n.CharPickerData.categories]. Encoding
   scheme is described in [goog.i18n.CharListDecompressor].
*)
  method charList : js_string t js_array t js_array t prop 

(**
   Subcategory names. Each subarray in this array is a list of subcategory
   names for the corresponding category specified in
   [goog.i18n.CharPickerData.categories].
*)
  method subcategories : js_string t js_array t js_array t prop     
end

val charPickerData : charPickerData t constr
