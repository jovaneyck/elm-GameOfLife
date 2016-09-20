module Util exposing (zip)


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)
