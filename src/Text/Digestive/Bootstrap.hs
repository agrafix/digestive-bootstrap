{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Bootstrap
    ( FormMeta (..), FormElement (..), FormElementCfg (..)
    , StdMethod (..)
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

data FormElementCfg
   = InputText
   | InputPassword
   | InputTextArea (Maybe Int) (Maybe Int)
   | InputHidden
   | InputSelect
   | InputRadio Bool
   | InputCheckbox
   | InputFile

data FormElement
   = FormElement
   { fe_name :: T.Text
   , fe_label :: Maybe T.Text
   , fe_cfg :: FormElementCfg
   }

data FormMeta
   = FormMeta
   { fm_method :: StdMethod
   , fm_target :: T.Text
   , fm_elements :: [FormElement]
   , fm_submitText :: T.Text
   }

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
             H.label ! for (toValue $ fe_name formElement) $ (toHtml lbl)
         Nothing ->
             mempty
       buildFun (fe_name formElement) formView ! class_ "form-control" ! placeholder (toValue $ fromMaybe "" $ fe_label formElement)
    where
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
