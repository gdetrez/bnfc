{-# LANGUAGE TemplateHaskell #-}
module BNFC.TemplateSpec where

import Test.Hspec
import Language.Haskell.TH

import BNFC.Template -- SUT

spec :: Spec
spec = do
  describe "template" $ do
    it "returns the content of a simple text file" $
      $(template "test/test-template.txt") `shouldBe` "abcd\n"

    it "interpolate a string variable" $
      let myvar = "Hello world" in
      $(template "test/test-template-interpolate.txt") `shouldBe` "print 'Hello world'\n"

  describe "Parsing Template" $ do
    it "simply retuns an empty lint for the empty string" $
      parseTemplate "" `shouldBe` []

    it "returns a string without code as one element" $
      parseTemplate "aoeu@$%@$%\n\n\n" `shouldBe` [LitE $ StringL "aoeu@$%@$%\n\n\n"]

    it "return interpolated code as haskell Exp" $
      parseTemplate "#{abc}" `shouldBe` [VarE (mkName "abc")]
