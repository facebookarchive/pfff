FBUNIT_DASHBOARD_EMAILS("foo@bar.com");

FBUNIT_TEST(testFlipByteOrderOfIp) {
  uint32_t flippedIp = flipByteOrderOfIp(167772160);
  EXPECT_EQ(flippedIp, 10);
}

