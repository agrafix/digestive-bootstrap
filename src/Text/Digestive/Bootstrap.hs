{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Bootstrap
    ( FormMeta (..), FormElement (..), FormElementCfg (..)
    , StdMethod (..)
    , renderForm
    )
where

import Data.Maybe
#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid
#endif
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
   { fe_name :: T.Text
   , fe_label :: Maybe T.Text
   , fe_cfg :: FormElementCfg
   }

-- | Meta information for a HTML form
data FormMeta
   = FormMeta
   { fm_method :: StdMethod
   , fm_target :: T.Text
   , fm_elements :: [FormElement]
   , fm_submitText :: T.Text
   }

-- | Render a form defined by 'FormMeta' information and
-- the digestive functor 'View'.
renderForm :: FormMeta -> View Html -> Html
renderForm formMeta formView =
    H.form ! role "form" ! method formMethod ! action formAction $
     do mapM_ (renderElement formView) (fm_elements formMeta)
        formSubmit (toHtml $ fm_submitText formMeta)
    where
      formMethod = toValue (T.decodeUtf8 $ renderStdMethod (fm_method formMeta))
      formAction = toValue $ fm_target formMeta

renderElement :: View Html -> FormElement -> Html
renderElement formView formElement =
    formGroup $
    do case errors (fe_name formElement) formView of
         [] -> mempty
         errorMsgs ->
             alertBox BootAlertDanger $ H.ul $ mapM_ (H.li . toHtml) errorMsgs
       case fe_label formElement of
         Just lbl ->
             H.label ! for (toValue $ fe_name formElement) $ toHtml lbl
         Nothing ->
             mempty
       let ct = buildFun (fe_name formElement) formView ! class_ "form-control" ! placeholder (toValue $ fromMaybe "" $ fe_label formElement)
       if hasAddon
       then H.div ! class_ "input-group" $ (ct >>= \_ -> groupAddonAfter)
       else ct
    where
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
