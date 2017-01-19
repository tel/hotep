
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Testing framework" $ do
    it "runs" $ do
      True `shouldBe` True
