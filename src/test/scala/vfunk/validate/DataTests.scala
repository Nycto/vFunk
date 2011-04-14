package test.scala.vfunk.validate

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }

import main.scala.vfunk.validate._

@RunWith(classOf[JUnitSuiteRunner])
class DataTests extends Specification with JUnit {

    "An Email Validator" should {
        val validator = new Email
        "pass for valid email addresses" in {
            validator must validateFor(
                "abc@example.com", "Abc@example.com",
                "aBC@example.com", "abc.123@example.com",
                "abc.123@sub.example.com", "abc.123@sub.sub.example.com",
                "abc+123@example.com", "1234567890@example.com",
                "_______@example.com", "test?mail@example.com",
                "abc+mailbox/department=shipping@example.com",
                "test=mail@example.com", "test-mail@example.com",
                "customer/department=shipping@example.com",
                "$A12345@example.com", "!def!xyz%abc@example.com",
                "_somename@example.com"
            )
        }
        "fail for invalid email addresses" in {
            validator must notValidateFor(
                "", "   ", "@", "Abc.example.com",
                "A@b@c@example.com", "Abc..123@example.com",
                "Abc. 123@ example.com", "Abc.\n123@example.com",
                "Abc.\r123@example.com", "Abc123@\texample.com",
                "()[];:,<>@example.com", "Abc@example.com.",
                ".Abc@example.com", "Abc.@example.com",
                "Abc@.example.com", "@example.com", "abc@",
                "abc@example", "foo@-foo.com", "foo@foo-.com"
            )
        }
    }
    "An IPv4 Validator" should {
        val validator = new IPv4
        "pass for valid IP addresses" in {
            validator must validateFor(
                "0.0.0.0", "127.0.0.1", "1.1.1.1",
                "255.255.255.255"
            )
        }
        "fail for invalid IP addresses" in {
            validator must notValidateFor(
                "", "   ", "127.127.127.",
                "  1.1.1.1   ", "1.1.1", "1.1.1.1.",
                "01.02.03.04"
            )
        }
    }

    /**
     * Tests extracted from:
     * http://download.dartware.com/thirdparty/test-ipv6-regex.pl
     * Copyright Rich Brown <richard.e.brown@dartware.com> 25 Feb 2010
     */
    "An IPv6 Validator" should {
        val validator = new IPv6
        "pass for valid IP addresses" in {
            validator must validateFor(
                "2001:0000:1234:0000:0000:C1C0:ABCD:0876",
                "3ffe:0b00:0000:0000:0001:0000:0000:000a",
                "FF02:0000:0000:0000:0000:0000:0000:0001",
                "0000:0000:0000:0000:0000:0000:0000:0001",
                "0000:0000:0000:0000:0000:0000:0000:0000",
                "::ffff:192.168.1.26", "2::10", "::ffff:0:0",
                "2001:0000:1234:0000:0000:C1C0:ABCD:0876",
                "ff02::1", "fe80::", "2002::", "2001:db8::",
                "::1", "::ffff:192.168.1.1", "1:2:3:4:5:6:7:8",
                "1:2:3:4:5:6::8", "1:2:3:4:5::8", "1:2:3:4::8",
                "1:2:3::8", "1:2::8", "1::8", "1::2:3:4:5:6:7",
                "1::2:3:4:5:6", "1::2:3:4:5", "1::2:3:4", "1::2:3",
                "1::8", "::2:3:4:5:6:7:8", "::2:3:4:5:6:7", "::2:3:4:5:6",
                "::2:3:4:5", "::2:3:4", "::2:3", "::8", "1:2:3:4:5:6::",
                "1:2:3:4:5::", "1:2:3:4::", "1:2:3::", "1:2::", "1::",
                "1:2:3:4:5::7:8", "1:2:3:4::7:8", "1:2:3::7:8", "1:2::7:8",
                "1::7:8", "1:2:3:4:5:6:1.2.3.4", "1:2:3:4:5::1.2.3.4",
                "1:2:3:4::1.2.3.4", "1:2:3::1.2.3.4", "1:2::1.2.3.4",
                "1::1.2.3.4", "1:2:3:4::5:1.2.3.4", "1:2:3::5:1.2.3.4",
                "1:2::5:1.2.3.4", "1::5:1.2.3.4", "1::5:11.22.33.44",
                "fe80::217:f2ff:254.7.237.98", "fe80::217:f2ff:fe07:ed62",
                "2001:DB8:0:0:8:800:200C:417A", "FF01:0:0:0:0:0:0:101",
                "0:0:0:0:0:0:0:1", "0:0:0:0:0:0:0:0", "::13.1.68.3",
                "2001:DB8::8:800:200C:417A", "FF01::101", "::1", "::",
                "0:0:0:0:0:0:13.1.68.3", "0:0:0:0:0:FFFF:129.144.52.38",
                "fe80:0000:0000:0000:0204:61ff:fe9d:f156",
                "fe80:0:0:0:204:61ff:fe9d:f156", "fe80::204:61ff:fe9d:f156",
                "fe80:0:0:0:204:61ff:254.157.241.86",
                "fe80::204:61ff:254.157.241.86", "::FFFF:129.144.52.38",
                "::1", "fe80::", "fe80::1", "2001:0db8:1234::",
                "2001:0db8:85a3:0000:0000:8a2e:0370:7334",
                "2001:db8:85a3:0:0:8a2e:370:7334",
                "2001:db8:85a3::8a2e:370:7334",
                "2001:0db8:0000:0000:0000:0000:1428:57ab",
                "2001:0db8:0000:0000:0000::1428:57ab",
                "2001:0db8:0:0:0:0:1428:57ab", "2001:0db8:0:0::1428:57ab",
                "2001:0db8::1428:57ab", "2001:db8::1428:57ab",
                "0000:0000:0000:0000:0000:0000:0000:0001",
                "::1", "::ffff:12.34.56.78", "::ffff:0c22:384e",
                "2001:0db8:1234:0000:0000:0000:0000:0000",
                "2001:0db8:1234:ffff:ffff:ffff:ffff:ffff",
                "2001:db8:a::123", "fe80::", "::ffff:192.0.2.128",
                "::ffff:c000:280", "1111:2222:3333:4444:5555:6666:7777:8888",
                "1111:2222:3333:4444:5555:6666:7777::",
                "1111:2222:3333:4444:5555:6666::", "1111:2222:3333:4444:5555::",
                "1111:2222:3333:4444::", "1111:2222:3333::", "1111:2222::",
                "1111::", "::", "1111:2222:3333:4444:5555:6666::8888",
                "1111:2222:3333:4444:5555::8888", "1111:2222:3333:4444::8888",
                "1111:2222:3333::8888", "1111:2222::8888", "1111::8888",
                "::8888", "1111:2222:3333:4444:5555::7777:8888",
                "1111:2222:3333:4444::7777:8888", "1111:2222:3333::7777:8888",
                "1111:2222::7777:8888", "1111::7777:8888", "::7777:8888",
                "1111:2222:3333:4444::6666:7777:8888",
                "1111:2222:3333::6666:7777:8888", "1111:2222::6666:7777:8888",
                "1111::6666:7777:8888", "::6666:7777:8888",
                "1111:2222:3333::5555:6666:7777:8888",
                "1111:2222::5555:6666:7777:8888", "1111::5555:6666:7777:8888",
                "::5555:6666:7777:8888", "1111:2222::4444:5555:6666:7777:8888",
                "1111::4444:5555:6666:7777:8888", "::4444:5555:6666:7777:8888",
                "1111::3333:4444:5555:6666:7777:8888",
                "::3333:4444:5555:6666:7777:8888",
                "::2222:3333:4444:5555:6666:7777:8888",
                "1111:2222:3333:4444:5555:6666:123.123.123.123",
                "1111:2222:3333:4444:5555::123.123.123.123",
                "1111:2222:3333:4444::123.123.123.123",
                "1111:2222:3333::123.123.123.123",
                "1111:2222::123.123.123.123", "1111::123.123.123.123",
                "::123.123.123.123", "::6666:123.123.123.123",
                "1111:2222:3333:4444::6666:123.123.123.123",
                "1111:2222:3333::6666:123.123.123.123",
                "1111:2222::6666:123.123.123.123", "1111::6666:123.123.123.123",
                "1111:2222:3333::5555:6666:123.123.123.123",
                "1111:2222::5555:6666:123.123.123.123",
                "1111::5555:6666:123.123.123.123",
                "::5555:6666:123.123.123.123",
                "1111:2222::4444:5555:6666:123.123.123.123",
                "1111::4444:5555:6666:123.123.123.123",
                "::4444:5555:6666:123.123.123.123",
                "1111::3333:4444:5555:6666:123.123.123.123",
                "::2222:3333:4444:5555:6666:123.123.123.123",
                "0:a:b:c:d:e:f::", "a:b:c:d:e:f:0::"
            )
        }
        "fail for invalid IP addresses" in {
            validator must notValidateFor(
                "", "   ", "127.127.127.", "::0:a:b:c:d:e:f ",
                "  1.1.1.1   ", "1.1.1", "1.1.1.1.", "::01.02.03.04",
                " 2001:0000:1234:0000:0000:C1C0:ABCD:0876",
                " 2001:0000:1234:0000:0000:C1C0:ABCD:0876  ",
                "02001:0000:1234:0000:0000:C1C0:ABCD:0876",
                "2001:0000:1234:0000:00001:C1C0:ABCD:0876",
                "2001:0DB8:0000:CD30:0000:0000:0000:0000/60",
                "2001:0DB8::CD30:0:0:0:0/60", "2001:0DB8:0:CD30::/60",
                "::/128", "::1/128", "FF00::/8", "FE80::/10", "FEC0::/10",
                "02001:0000:1234:0000:0000:C1C0:ABCD:0876",
                "2001:0000:1234:0000:00001:C1C0:ABCD:0876",
                " 2001:0000:1234:0000:0000:C1C0:ABCD:0876  0",
                "2001:0000:1234: 0000:0000:C1C0:ABCD:0876",
                "2001:1:1:1:1:1:255Z255X255Y255",
                "::ffff:192x168.1.26", "3ffe:0b00:0000:0001:0000:0000:000a",
                "FF02:0000:0000:0000:0000:0000:0000:0000:0001",
                "3ffe:b00::1::a", "::1111:2222:3333:4444:5555:6666::",
                "1:2:3::4:5::7:8", "12345::6:7:8", "1::5:400.2.3.4",
                "1::5:260.2.3.4", "1::5:256.2.3.4", "1::5:1.256.3.4",
                "1::5:1.2.256.4", "1::5:1.2.3.256", "1::5:300.2.3.4",
                "1::5:1.300.3.4", "1::5:1.2.300.4", "1::5:1.2.3.300",
                "1::5:900.2.3.4", "1::5:1.900.3.4", "1::5:1.2.900.4",
                "1::5:1.2.3.900", "1::5:300.300.300.300", "1::5:3000.30.30.30",
                "1::400.2.3.4", "1::260.2.3.4", "1::256.2.3.4", "1::1.256.3.4",
                "1::1.2.256.4", "1::1.2.3.256", "1::300.2.3.4", "1::1.300.3.4",
                "1::1.2.300.4", "1::1.2.3.300", "1::900.2.3.4", "1::1.900.3.4",
                "1::1.2.900.4", "1::1.2.3.900", "1::300.300.300.300",
                "1::3000.30.30.30", "::400.2.3.4", "::260.2.3.4", "::256.2.3.4",
                "::1.256.3.4", "::1.2.256.4", "::1.2.3.256", "::300.2.3.4",
                "::1.300.3.4", "::1.2.300.4", "::1.2.3.300", "::900.2.3.4",
                "::1.900.3.4", "::1.2.900.4", "::1.2.3.900",
                "::300.300.300.300", "::3000.30.30.30", "124.15.6.89/60",
                "2001:DB8:0:0:8:800:200C:417A:221", "FF01::101::2",
                "fe80:0000:0000:0000:0204:61ff:254.157.241.086", ":",
                "1111:2222:3333:4444::5555:", "1111:2222:3333::5555:",
                "1111:2222::5555:", "1111::5555:", "::5555:", ":::",
                "1111:", ":", ":1111:2222:3333:4444::5555",
                ":1111:2222:3333::5555", ":1111:2222::5555", ":1111::5555",
                ":::5555", ":::", "1.2.3.4:1111:2222:3333:4444::5555",
                "1.2.3.4:1111:2222:3333::5555", "1.2.3.4:1111:2222::5555",
                "1.2.3.4:1111::5555", "1.2.3.4::5555", "1.2.3.4::",
                "123", "ldkfj", "2001::FFD3::57ab", "1:2:3:4:5:6:7:8:9",
                "2001:db8:85a3::8a2e:370k:7334", "1::2::3", "1:::3:4:5",
                "2001:db8:85a3::8a2e:37023:7334", "1:2:3::4:5:6:7:8:9",
                "::ffff:2.3.4", "::ffff:257.1.2.3", "1.2.3.4",
                "XXXX:XXXX:XXXX:XXXX:XXXX:XXXX:XXXX:XXXX",
                "1111:2222:3333:4444:5555:6666:7777:8888:9999",
                "1111:2222:3333:4444:5555:6666:7777:8888::",
                "::2222:3333:4444:5555:6666:7777:8888:9999",
                "1111:2222:3333:4444:5555:6666:7777",
                "1111:2222:3333:4444:5555:6666", "1111:2222:3333",
                "1111:2222:3333:4444:5555", "1111:2222:3333:4444",
                "1111:2222", "1111", "11112222:3333:4444:5555:6666:7777:8888",
                "1111:22223333:4444:5555:6666:7777:8888",
                "1111:2222:33334444:5555:6666:7777:8888",
                "1111:2222:3333:44445555:6666:7777:8888",
                "1111:2222:3333:4444:55556666:7777:8888",
                "1111:2222:3333:4444:5555:66667777:8888",
                "1111:2222:3333:4444:5555:6666:77778888",
                "1111:2222:3333:4444:5555:6666:7777:8888:",
                "1111:2222:3333:4444:5555:6666:7777:",
                "1111:2222:3333:4444:5555:6666:",
                "1111:2222:3333:4444:5555:", "1111:2222:3333:4444:",
                "1111:2222:3333:", "1111:2222:", "1111:", ":", ":8888",
                ":7777:8888", ":6666:7777:8888", ":5555:6666:7777:8888",
                ":4444:5555:6666:7777:8888", ":3333:4444:5555:6666:7777:8888",
                ":2222:3333:4444:5555:6666:7777:8888",
                ":1111:2222:3333:4444:5555:6666:7777:8888",
                ":::2222:3333:4444:5555:6666:7777:8888",
                "1111:::3333:4444:5555:6666:7777:8888",
                "1111:2222:::4444:5555:6666:7777:8888",
                "1111:2222:3333:::5555:6666:7777:8888",
                "1111:2222:3333:4444:::6666:7777:8888",
                "1111:2222:3333:4444:5555:::7777:8888",
                "1111:2222:3333:4444:5555:6666:::8888",
                "1111:2222:3333:4444:5555:6666:7777:::",
                "::2222::4444:5555:6666:7777:8888",
                "::2222:3333::5555:6666:7777:8888",
                "::2222:3333:4444::6666:7777:8888",
                "::2222:3333:4444:5555::7777:8888",
                "::2222:3333:4444:5555:7777::8888",
                "::2222:3333:4444:5555:7777:8888::",
                "1111::3333::5555:6666:7777:8888",
                "1111::3333:4444::6666:7777:8888",
                "1111::3333:4444:5555::7777:8888",
                "1111::3333:4444:5555:6666::8888",
                "1111::3333:4444:5555:6666:7777::",
                "1111:2222::4444::6666:7777:8888",
                "1111:2222::4444:5555::7777:8888",
                "1111:2222::4444:5555:6666::8888",
                "1111:2222::4444:5555:6666:7777::",
                "1111:2222:3333::5555::7777:8888",
                "1111:2222:3333::5555:6666::8888",
                "1111:2222:3333::5555:6666:7777::",
                "1111:2222:3333:4444::6666::8888",
                "1111:2222:3333:4444::6666:7777::",
                "1111:2222:3333:4444:5555::7777::",
                "XXXX:XXXX:XXXX:XXXX:XXXX:XXXX:1.2.3.4",
                "1111:2222:3333:4444:5555:6666:00.00.00.00",
                "1111:2222:3333:4444:5555:6666:000.000.000.000",
                "1111:2222:3333:4444:5555:6666:256.256.256.256",
                "1111:2222:3333:4444:5555:6666:7777:8888:1.2.3.4",
                "1111:2222:3333:4444:5555:6666:7777:1.2.3.4",
                "1111:2222:3333:4444:5555:6666::1.2.3.4",
                "::2222:3333:4444:5555:6666:7777:1.2.3.4",
                "1111:2222:3333:4444:5555:6666:1.2.3.4.5",
                "1111:2222:3333:4444:5555:1.2.3.4",
                "1111:2222:3333:4444:1.2.3.4", "1111:2222:3333:1.2.3.4",
                "1111:2222:1.2.3.4", "1111:1.2.3.4", "1.2.3.4",
                "11112222:3333:4444:5555:6666:1.2.3.4",
                "1111:22223333:4444:5555:6666:1.2.3.4",
                "1111:2222:33334444:5555:6666:1.2.3.4",
                "1111:2222:3333:44445555:6666:1.2.3.4",
                "1111:2222:3333:4444:55556666:1.2.3.4",
                "1111:2222:3333:4444:5555:66661.2.3.4",
                "1111:2222:3333:4444:5555:6666:255255.255.255",
                "1111:2222:3333:4444:5555:6666:255.255255.255",
                "1111:2222:3333:4444:5555:6666:255.255.255255",
                ":1.2.3.4", ":6666:1.2.3.4", ":5555:6666:1.2.3.4",
                ":4444:5555:6666:1.2.3.4", ":3333:4444:5555:6666:1.2.3.4",
                ":2222:3333:4444:5555:6666:1.2.3.4",
                ":1111:2222:3333:4444:5555:6666:1.2.3.4",
                ":::2222:3333:4444:5555:6666:1.2.3.4",
                "1111:::3333:4444:5555:6666:1.2.3.4",
                "1111:2222:::4444:5555:6666:1.2.3.4",
                "1111:2222:3333:::5555:6666:1.2.3.4",
                "1111:2222:3333:4444:::6666:1.2.3.4",
                "1111:2222:3333:4444:5555:::1.2.3.4",
                "::2222::4444:5555:6666:1.2.3.4",
                "::2222:3333::5555:6666:1.2.3.4",
                "::2222:3333:4444::6666:1.2.3.4",
                "::2222:3333:4444:5555::1.2.3.4",
                "1111::3333::5555:6666:1.2.3.4",
                "1111::3333:4444::6666:1.2.3.4",
                "1111::3333:4444:5555::1.2.3.4",
                "1111:2222::4444::6666:1.2.3.4",
                "1111:2222::4444:5555::1.2.3.4",
                "1111:2222:3333::5555::1.2.3.4",
                "::.", "::..", "::...", "::1...", "::1.2..",
                "::1.2.3.", "::.2..", "::.2.3.", "::.2.3.4",
                "::..3.", "::..3.4", "::...4",
                ":1111:2222:3333:4444:5555:6666:7777::",
                ":1111:2222:3333:4444:5555:6666::",
                ":1111:2222:3333:4444:5555::",
                ":1111:2222:3333:4444::", ":1111:2222:3333::",
                ":1111:2222::", ":1111::", ":::",
                ":1111:2222:3333:4444:5555:6666::8888",
                ":1111:2222:3333:4444:5555::8888",
                ":1111:2222:3333:4444::8888",
                ":1111:2222::8888", ":1111::8888", ":::8888",
                ":1111:2222:3333:4444:5555::7777:8888",
                ":1111:2222:3333:4444::7777:8888",
                ":1111:2222:3333::7777:8888", ":1111:2222::7777:8888",
                ":1111::7777:8888", ":::7777:8888",
                ":1111:2222:3333:4444::6666:7777:8888",
                ":1111:2222:3333::6666:7777:8888",
                ":1111:2222::6666:7777:8888", ":1111:2222:3333::8888",
                ":1111::6666:7777:8888", ":::6666:7777:8888",
                ":1111:2222:3333::5555:6666:7777:8888",
                ":1111:2222::5555:6666:7777:8888",
                ":1111::5555:6666:7777:8888", ":::5555:6666:7777:8888",
                ":1111:2222::4444:5555:6666:7777:8888",
                ":1111::4444:5555:6666:7777:8888",
                ":::4444:5555:6666:7777:8888",
                ":1111::3333:4444:5555:6666:7777:8888",
                ":::3333:4444:5555:6666:7777:8888",
                ":::2222:3333:4444:5555:6666:7777:8888",
                ":1111:2222:3333:4444:5555:6666:1.2.3.4",
                ":1111:2222:3333:4444:5555::1.2.3.4",
                ":1111:2222:3333:4444::1.2.3.4", ":::1.2.3.4",
                ":1111:2222:3333::1.2.3.4", ":1111:2222::1.2.3.4",
                ":1111::1.2.3.4", ":1111:2222:3333:4444::6666:1.2.3.4",
                ":1111:2222:3333::6666:1.2.3.4",
                ":1111:2222::6666:1.2.3.4", ":1111::6666:1.2.3.4",
                ":::6666:1.2.3.4", ":1111:2222:3333::5555:6666:1.2.3.4",
                ":1111:2222::5555:6666:1.2.3.4", ":1111::5555:6666:1.2.3.4",
                ":::5555:6666:1.2.3.4", ":1111:2222::4444:5555:6666:1.2.3.4",
                ":1111::4444:5555:6666:1.2.3.4", ":::4444:5555:6666:1.2.3.4",
                ":1111::3333:4444:5555:6666:1.2.3.4",
                ":::2222:3333:4444:5555:6666:1.2.3.4",
                "1111:2222:3333:4444:5555:6666:7777:::",
                "1111:2222:3333:4444:5555:6666:::",
                "1111:2222:3333:4444:5555:::", "1111:2222:3333:4444:::",
                "1111:2222:3333:::", "1111:2222:::", "1111:::",
                ":::", "1111:2222:3333:4444:5555:6666::8888:",
                "1111:2222:3333:4444:5555::8888:",
                "1111:2222:3333:4444::8888:", "1111:2222:3333::8888:",
                "1111:2222::8888:", "1111::8888:", "::8888:",
                "1111:2222:3333:4444:5555::7777:8888:",
                "1111:2222:3333:4444::7777:8888:",
                "1111:2222:3333::7777:8888:", "1111:2222::7777:8888:",
                "1111::7777:8888:", "::7777:8888:",
                "1111:2222:3333:4444::6666:7777:8888:",
                "1111:2222:3333::6666:7777:8888:",
                "1111:2222::6666:7777:8888:",
                "1111::6666:7777:8888:", "::6666:7777:8888:",
                "1111:2222:3333::5555:6666:7777:8888:",
                "1111:2222::5555:6666:7777:8888:",
                "1111::5555:6666:7777:8888:", "::5555:6666:7777:8888:",
                "1111:2222::4444:5555:6666:7777:8888:",
                "1111::4444:5555:6666:7777:8888:",
                "::4444:5555:6666:7777:8888:",
                "1111::3333:4444:5555:6666:7777:8888:",
                "::3333:4444:5555:6666:7777:8888:",
                "::2222:3333:4444:5555:6666:7777:8888:", "':10.0.0.1",
                "1fff::a88:85a3::172.31.128.1", "0:0:0:255.255.255.25"
            )
        }
    }
}
