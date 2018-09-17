
context("texNum return correct")

expect_equal(texNum(180000), "180,000")
expect_equal(texNum(1800000), "1.8~million")
expect_equal(texNum(1850000), "1.85~million")
expect_equal(texNum(1850000, 2), "1.8~million")
expect_equal(texNum(1850000, 2, TRUE), "\\$1.8~million")
expect_equal(texNum(-1850000, 2, TRUE), "$-$\\$1.8~million")
expect_equal(texNum(-5), "$-$5")
