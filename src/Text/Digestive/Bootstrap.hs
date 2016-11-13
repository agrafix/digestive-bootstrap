{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Bootstrap
    ( FormMeta (..), FormElement (..), FormElementCfg (..)
    , FormSection (..), FormComponent (..)
    , StdMethod (..), NumberUnit
    , renderForm
    )
where

import Data.Maybe
import Data.Monoid
import Network.HTTP.Types.Method
import Text.Blaze.Bootstrap
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import Text.Digestive
import Text.Digestive.Blaze.Html5
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type NumberUnit = T.Text

-- | Form element type
data FormElementCfg
   = InputText
   | InputNumber (Maybe NumberUnit)
   | InputPassword
   | InputTextArea (Maybe Int) (Maybe Int)
   | InputHidden
   | InputSelect
   | InputRadio Bool
   | InputCheckbox
   | InputFile
   | InputDate

-- | Configuration for a form element
data FormElement
   = FormElement
   { fe_name :: !T.Text
   , fe_label :: !(Maybe T.Text)
   , fe_cfg :: !FormElementCfg
   }

data FormSection
    = FormSection
    { fs_title :: !(Maybe T.Text)
    , fs_help :: !(Maybe T.Text)
    , fs_elements :: ![FormElement]
    }

data FormComponent
    = FCSection !FormSection
    | FCHtmlSection !H.Html

-- | Meta information for a HTML form
data FormMeta
   = FormMeta
   { fm_method :: StdMethod
   , fm_target :: T.Text
   , fm_components :: [FormComponent]
   , fm_submitValue :: Html
   }

-- | Render a form defined by 'FormMeta' information and
-- the digestive functor 'View'.
renderForm :: FormMeta -> View Html -> Html
renderForm formMeta formView =
    H.form ! role "form" ! method formMethod ! action formAction $
     do mapM_ (renderComponent formView) (fm_components formMeta)
        formSubmit (fm_submitValue formMeta)
    where
      formMethod = toValue (T.decodeUtf8 $ renderStdMethod (fm_method formMeta))
      formAction = toValue $ fm_target formMeta

renderComponent :: View Html -> FormComponent -> Html
renderComponent formView comp =
    case comp of
      FCSection fs -> renderSection formView fs
      FCHtmlSection bdy -> bdy

renderSection :: View Html -> FormSection -> Html
renderSection formView formSection =
    H.div ! class_ "form-section" $
    do case fs_title formSection of
         Nothing -> mempty
         Just x -> H.h3 ! class_ "form-section-title" $ toHtml x
       case fs_help formSection of
         Nothing -> mempty
         Just x -> H.p ! class_ "form-section-help" $ toHtml x
       mapM_ (renderElement formView) (fs_elements formSection)

renderElement :: View Html -> FormElement -> Html
renderElement formView formElement =
    wrapper $
    do case errors (fe_name formElement) formView of
         [] -> mempty
         errorMsgs ->
             alertBox BootAlertDanger $ H.ul ! class_ "form-errors" $ mapM_ (H.li . toHtml) errorMsgs
       case fe_cfg formElement of
         InputCheckbox ->
             H.label $
             do (inputCheckbox (fe_name formElement) formView) <> " "
                case fe_label formElement of
                  Nothing -> mempty
                  Just lbl -> H.toHtml lbl
         _ ->
             do case fe_label formElement of
                  Just lbl ->
                      H.label ! for (toValue $ fe_name formElement) $ toHtml lbl
                  Nothing ->
                      mempty
                let ct =
                        buildFun (fe_name formElement) formView
                        ! class_ "form-control"
                        ! placeholder (toValue . fromMaybe "" . fe_label $ formElement)
                if hasAddon
                then H.div ! class_ "input-group" $ (ct >>= \_ -> groupAddonAfter)
                else ct
    where
      wrapper x =
          case fe_cfg formElement of
            InputCheckbox -> H.div ! class_ "checkbox" $ x
            _ -> formGroup x
      (hasAddon, groupAddonAfter) =
          case fe_cfg formElement of
            InputNumber (Just numberUnit) ->
                (True, H.span ! class_ "input-group-addon" $ toHtml numberUnit)
            _ ->
                (False, mempty)
      buildFun =
          case fe_cfg formElement of
            InputText -> inputText
            InputPassword -> inputPassword
            InputTextArea taRows taCols -> inputTextArea taRows taCols
            InputHidden -> inputHidden
            InputSelect -> inputSelect
            InputRadio rBr -> inputRadio rBr
            InputCheckbox -> inputCheckbox
            InputFile -> inputFile
            InputNumber _ -> inputX "number"
            InputDate -> inputX "date"

inputX :: T.Text -> T.Text -> View v -> Html
inputX x ref view =
    input
    ! type_ (toValue x)
    ! A.id    (H.toValue ref')
    ! name  (H.toValue ref')
    ! value (H.toValue $ fieldInputText ref view)
    !? (x == "number", A.step "any")
  where
    ref' = absoluteRef ref view
