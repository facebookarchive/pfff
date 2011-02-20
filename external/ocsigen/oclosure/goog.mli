(*
  OClosure project - 2010

  OClosure is a binding of the Google Closure javascript library
  It uses the Js_of_ocaml compiler

  @author Gabriel Cardoso, Charles Oran
  @version 0.2 
n*)

(** Binding of the Google Closure javascript library

  @author Gabriel Cardoso, Charles Oran, Bozman Cagdas, Emmanuel Crespin, Esther Baruk, Pierre Bourdin
  @version 0.2
*)

#define GOOG
module Tools : sig
#include "tools.mli"
end
module Disposable : sig
#include "disposable/disposable.mli"
end
module Math : sig
#include "math/math.mli"
end
module Async : sig
#include "async/async.mli"
end
module Date : sig
#include "date/date.mli"
end
module Events : sig
#include "events/events.mli"
end
module Fx : sig
#include "fx/fx.mli"
end
module Gdom : sig
#include "gdom/gdom.mli"
end
module Geditor : sig
#include "geditor/geditor.mli"
end
module Ggraphics : sig
#include "ggraphics/ggraphics.mli"
end
module I18n : sig
#include "i18n/i18n.mli"
end
module Positioning : sig
#include "positioning/positioning.mli"
end
module Spell : sig
#include "spell/spell.mli"
end
module Structs : sig
#include "structs/structs.mli"
end
module Timer : sig
#include "timer/timer.mli"
end
module Ui : sig
#include "ui/ui.mli"
end
module UserAgent : sig
#include "userAgent/userAgent.mli"
end
