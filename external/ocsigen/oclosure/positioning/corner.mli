module Corner : sig
  type corner = 
      TOP_LEFT
    | BOTTOM_LEFT
    | TOP_RIGHT
    | BOTTOM_RIGHT
    | TOP_START
    | BOTTOM_START
    | TOP_END
    | BOTTOM_END
end

type corner = Corner.corner
