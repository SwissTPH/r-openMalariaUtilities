#!/usr/bin/env python3

import tempfile
import unittest

from python.xsd_parser import XSDparser


class TestXSDparser(unittest.TestCase):

    def test_docs_extractor(self):
        with tempfile.NamedTemporaryFile(delete_on_close=False, delete=False) as tmp:
            content = b"""<xs:schema xmlns:om="http://openmalaria.org/schema/scenario_47" xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://openmalaria.org/schema/scenario_47">
            <xs:element name="scenario">
                <xs:annotation>
                <xs:documentation>Description of scenario</xs:documentation>
                <xs:appinfo>name:Scenario;</xs:appinfo>
                </xs:annotation>
            </xs:element>
            </xs:schema>
            """
            tmp.write(content)
            tmp.close()
            print(tmp.name)
            xsd_init = XSDparser(47, str(tmp.name))
            self.assertEqual(xsd_init.om_version, "47", "OM version does not match.")

if __name__ == '__main__':
    unittest.main()
