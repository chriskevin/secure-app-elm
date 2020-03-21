module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , authenticated : Authenticated
    , view : View
    }


init : Model
init =
    Model "" "" "" Authorized ViewDashboard


signIn : String -> String -> String -> Bool
signIn name password passwordAgain =
    name /= "" && password == passwordAgain


type Authenticated
    = Authorized
    | Unauthorized


type View
    = ViewDashboard
    | ViewSignIn
    | ViewSignUp


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | ChangeView View
    | SignIn
    | SignOut


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeView nextView ->
            case model.authenticated of
                Authorized ->
                    case nextView of
                        ViewDashboard ->
                            { model | view = nextView }

                        ViewSignIn ->
                            model

                        ViewSignUp ->
                            model

                Unauthorized ->
                    case nextView of
                        ViewDashboard ->
                            model

                        ViewSignIn ->
                            { model | view = nextView }

                        ViewSignUp ->
                            { model | view = nextView }

        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain passwordAgain ->
            { model | passwordAgain = passwordAgain }

        SignIn ->
            if signIn model.name model.password model.passwordAgain then
                { model
                    | authenticated = Authorized
                    , name = ""
                    , password = ""
                    , passwordAgain = ""
                    , view = ViewDashboard
                }

            else
                { model | authenticated = Unauthorized }

        SignOut ->
            { model | authenticated = Unauthorized, view = ViewSignIn }


view : Model -> Html Msg
view model =
    case model.view of
        ViewDashboard ->
            viewDashboard model

        ViewSignIn ->
            viewSignIn model

        ViewSignUp ->
            viewSignUp model


viewDashboard : Model -> Html Msg
viewDashboard model =
    div []
        [ header []
            [ h1 [] [ text "Dashboard" ]
            , nav []
                [ text "Signed in"
                , button [ onClick SignOut ] [ text "Sign Out" ]
                ]
            ]
        , main_ [] []
        ]

signInBtnDisabled : String -> String -> Bool
signInBtnDisabled name password = name == "" && password == ""

viewSignUp : Model -> Html Msg
viewSignUp model =
    div [ id "form-register" ]
        [ main_ []
            [ fieldset []
                [ legend [] [ text "Sign Up" ]
                , label [ for "signup-name" ] [ text "Name" ]
                , viewInput "text" "signup-name" "Name" model.name Name
                , label [ for "signup-password" ] [ text "Password" ]
                , viewInput "text" "signup-password" "Password" model.password Password
                , label [ for "signup-password-again" ] [ text "Repeat password" ]
                , viewInput "text" "signup-password-again" "PasswordAgain" model.passwordAgain PasswordAgain
                , viewValidation model
                , button [ disabled (signInBtnDisabled model.name model.password), onClick SignIn ] [ text "Sign Up" ]
                , a [ href "#signin", onClick (ChangeView ViewSignIn) ] [ text "Back" ]
                ]
            ]
        ]


viewSignIn : Model -> Html Msg
viewSignIn model =
    div []
        [ main_ []
            [ fieldset []
                [ legend [] [ text "Sign In" ]
                , label [ for "login-name" ] [ text "Name" ]
                , viewInput "text" "login-name" "Name" model.name Name
                , label [ for "login-password" ] [ text "Password" ]
                , viewInput "text" "login-password" "Password" model.password Password
                , viewValidation model
                , button [ onClick SignIn ] [ text "Sign In" ]
                , a [ href "#signup", onClick (ChangeView ViewSignUp) ] [ text "Sign Up" ]
                ]
            ]
        ]


viewInput : String -> String -> String -> String -> (String -> msg) -> Html msg
viewInput t i p v toMsg =
    input [ type_ t, id i, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if model.password == model.passwordAgain then
        div [ style "color" "green" ] [ text "OK" ]

    else
        div [ style "color" "red" ] [ text "Password do not match!" ]
