{-# LANGUAGE TemplateHaskell #-}

module System.Mail.Notmuch.Wrapper.Template
       ( makeManualOrGB, makeGBDepData
       ) where

import Language.Haskell.TH
import Foreign
import Data.Monoid

makeOptionnalyHandledData :: String -> Q [Dec]
makeOptionnalyHandledData str = return . return $
    DataD [] (mkName str) [PlainTV $ mkName "s"] Nothing
          [ NormalC (mkName $ "Manual" <> str) [ ( Bang NoSourceUnpackedness SourceStrict
                                                 , ConT $ mkName $ "C" <> str
                                                 )
                                               ]
          , NormalC (mkName $ "GB" <> str) [ ( Bang NoSourceUnpackedness SourceStrict
                                             , AppT (ConT $ mkName "ForeignPtr")
                                                  $ ConT $ mkName $ "C" <> str
                                             )
                                           ]
          ]
          []

makeWrapperWithDestructor :: String -> String -> Q [Dec]
makeWrapperWithDestructor name destr = do
    body <- [|fmap $(return $ ConE $ mkName $ "GB" <> name)
                   . newForeignPtr $(return $ VarE $ mkName destr)
                   . $(return $ VarE $ mkName $ "unC" <> name)
            |]
    tp <- [t|$(mkConT $ "C" <> name) -> IO ($(mkConT name) $(mkVarT "s"))|]
    return $ [ SigD (mkName $ "makeGB" <> name) tp
             , FunD (mkName $ "makeGB" <> name) [ Clause [] (NormalB body) [] ]
             ]

mkConT :: String -> Q Type
mkConT = return . ConT . mkName
mkVarT :: String -> Q Type
mkVarT = return . VarT . mkName
mkVarE :: String -> Q Exp
mkVarE = return . VarE . mkName
mkConE :: String -> Q Exp
mkConE = return . ConE . mkName

makeWithWrapper :: String -> Q [Dec]
makeWithWrapper name = do
    body <- [| withForeignPtr $(return $ VarE $ mkName "ptr")
               $ $(return $ VarE $ mkName "f") . $(return $ ConE $ mkName $ "C" <> name)
            |]
    tp <- [t| ($(mkConT name) $(mkVarT "s"))
           -> ($(mkConT $ "C" <> name) -> IO $(mkVarT "a"))
           -> IO $(mkVarT "a") |] 
    return $ [ SigD (mkName $ "withC" <> name) tp
             , FunD (mkName $ "withC" <> name)
                    [ Clause [ ConP (mkName $ "Manual" <> name) [VarP $ mkName "cdata"]
                             , VarP $ mkName "f"
                             ]
                             (NormalB $ AppE (VarE $ mkName "f") (VarE $ mkName "cdata"))
                             []
                    , Clause [ ConP (mkName $ "GB" <> name) [VarP $ mkName "ptr"]
                             , VarP $ mkName "f"
                             ]
                             (NormalB body)
                             []
                    ]
             ]

makeManualOrGB :: String -> String -> Q [Dec]
makeManualOrGB name destr = fmap mconcat $ sequence
                          [ makeOptionnalyHandledData name
                          , makeWrapperWithDestructor name destr
                          , makeWithWrapper           name
                          ]

makeGBWithDep :: String -> String -> Q [Dec]
makeGBWithDep name dep = return . return $
    DataD [] (mkName name) [PlainTV $ mkName "s"] Nothing
          [ NormalC (mkName name) [ ( Bang NoSourceUnpackedness SourceStrict
                                    , AppT (ConT $ mkName "ForeignPtr")
                                           (ConT $ mkName $ "C" <> name)
                                    )
                                  , ( Bang NoSourceUnpackedness SourceStrict
                                    , AppT (ConT $ mkName dep)
                                           (VarT $ mkName "s")
                                    )
                                  ]
          ]
          []

makeGBDep :: String -> String -> String -> Q [Dec]
makeGBDep name dep destr = do
    tp <- [t|($(mkConT dep) $(mkVarT "s"))
          -> $(mkConT $ "C" <> name)
          -> IO ($(mkConT name) $(mkVarT "s"))
          |]
    body <- [| newForeignPtr $(mkVarE destr) ($(mkVarE $ "unC" <> name) $(mkVarE "cdata"))
           >>= \ptr -> return $ $(mkConE name) ptr $(mkVarE "dep")
            |]
    return [ SigD (mkName $ "make" <> name) tp
           , FunD (mkName $ "make" <> name)
                  [ Clause [ VarP $ mkName "dep", VarP $ mkName "cdata" ]
                           (NormalB body)
                           []
                  ]
           ]

makeWithGBDep :: String -> Q [Dec]
makeWithGBDep name = do
    tp <- [t|($(mkConT name) $(mkVarT "s"))
          -> ($(mkConT $ "C" <> name) -> IO $(mkVarT "a"))
          -> IO $(mkVarT "a")
          |]
    body <- [| withForeignPtr $(mkVarE "ptr")
             $ $(mkVarE "f") . $(mkConE $ "C" <> name)
            |]
    return [ SigD (mkName $ "withC" <> name) tp
           , FunD (mkName $ "withC" <> name)
                  [ Clause [ ConP (mkName name) [VarP $ mkName "ptr", WildP]
                           , VarP $ mkName "f"
                           ]
                           (NormalB body)
                           []
                  ]
           ]

makeGBDepData :: String -> String -> String -> Q [Dec]
makeGBDepData name dep destr = fmap mconcat $ sequence
                             [ makeGBWithDep name dep
                             , makeGBDep     name dep destr
                             , makeWithGBDep name
                             ]
