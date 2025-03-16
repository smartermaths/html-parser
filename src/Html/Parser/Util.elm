module Html.Parser.Util exposing (toVirtualDom)

{-| Utility functions that may help you digging into the contents.


# Virtual DOM

@docs toVirtualDom

-}

import Base64
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Parser exposing (Node(..))
import SvgParser


{-| Converts nodes to virtual dom nodes.
-}
toVirtualDom : List Node -> List (Html msg)
toVirtualDom nodes =
    List.map toVirtualDomEach nodes


toVirtualDomEach : Node -> Html msg
toVirtualDomEach node =
    case node of
        Element name attrs children ->
            -- BEGIN MODIFICATION OF PACKAGE 2021-05-11 mjd SmarterMaths
            -- Note: this is the only change (plus the inclusion of import Base64 and import SvgParser) as at 2021-05-11
            --       and assumes that svg elements have previously been transformed into
            --       p elements with a class smsvg with the previous svg element encoded as base64 and included in the p text
            --       It also assumes that the Base64 and SvgParser have been included in the elm.json as has this localElmPackages dir in srcs 
            if (name == "p" && (List.member ("class", "smsvg") attrs)) then
                children
                    |> List.map Html.Parser.nodeToString 
                    |> List.foldl (++) "" 
                    |> Base64.decode
                    |> Result.andThen SvgParser.parse
                    |> Result.withDefault (text "error")

            else
                -- END MODIFICATION OF PACKAGE 2021-05-11 mjd SmarterMaths
                Html.node name (List.map toAttribute attrs) (toVirtualDom children)

        Text s ->
            text s

        Comment _ ->
            text ""


toAttribute : ( String, String ) -> Attribute msg
toAttribute ( name, value ) =
    attribute name value
