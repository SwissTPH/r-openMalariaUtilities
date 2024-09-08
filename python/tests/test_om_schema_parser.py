#!/usr/bin/env python3

import tempfile
import unittest

from om_schema.parser import XSDparser


class TestXSDparser(unittest.TestCase):
    def test_init(self):
        with tempfile.NamedTemporaryFile(delete_on_close=False) as tmp:
            content = b"""
            <xs:schema xmlns:om="http://openmalaria.org/schema/scenario_47" xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://openmalaria.org/schema/scenario_47">
            </xs:schema>
            """
            tmp.write(content)
            tmp.close()

            xsd_init = XSDparser(47, str(tmp.name))
            # These are trivial tests, but we do them anyways
            self.assertEqual(xsd_init.om_version, "47", "OM version does not match.")
            self.assertEqual(
                xsd_init.xsd_source, str(tmp.name), "XSD source does not match."
            )
            self.assertEqual(
                xsd_init.namespaces,
                {
                    "om": "http://openmalaria.org/schema/scenario_47",
                    "xs": "http://www.w3.org/2001/XMLSchema",
                },
                "namespaces do not match.",
            )
            self.assertEqual(
                xsd_init.xs_ns,
                "{http://www.w3.org/2001/XMLSchema}",
                "xs namespace does not match.",
            )

    def test_docs_extractor(self):
        with tempfile.NamedTemporaryFile(delete_on_close=False) as tmp:
            content = b"""
            <xs:schema xmlns:om="http://openmalaria.org/schema/scenario_47" xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://openmalaria.org/schema/scenario_47">
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

            xsd_init = XSDparser(47, str(tmp.name))
            for node in xsd_init.xml_root.iter(f"{xsd_init.xs_ns}element"):
                self.assertEqual(
                    xsd_init.docs_extractor(node=node, namespaces=xsd_init.namespaces),
                    "\nDescription of scenario\nname:\tScenario",
                )

    def test_attr_extractor(self):
        with tempfile.NamedTemporaryFile(delete_on_close=False) as tmp:
            content = b"""
            <xs:schema xmlns:om="http://openmalaria.org/schema/scenario_47" xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://openmalaria.org/schema/scenario_47">
                <xs:element name="scenario" foo="1" bar ="2" baz="3">
                </xs:element>
            </xs:schema>
            """
            tmp.write(content)
            tmp.close()

            xsd_init = XSDparser(47, str(tmp.name))
            for node in xsd_init.xml_root.iter(f"{xsd_init.xs_ns}element"):
                self.assertEqual(
                    xsd_init.attr_extractor(node=node),
                    {"foo": "1", "bar": "2", "baz": "3"},
                )

    def test_handle_restrictions(self):
        with tempfile.NamedTemporaryFile(delete_on_close=False) as tmp:
            content = b"""
            <xs:schema xmlns:om="http://openmalaria.org/schema/scenario_47" xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://openmalaria.org/schema/scenario_47">
                <xs:restriction base="xs:string">
                    <xs:enumeration value="initial"/>
                    <xs:enumeration value="tracking"/>
                </xs:restriction>
            </xs:schema>
            """
            tmp.write(content)
            tmp.close()

            xsd_init = XSDparser(47, str(tmp.name))
            for node in xsd_init.xml_root.iter(f"{xsd_init.xs_ns}restriction"):
                self.assertEqual(
                    xsd_init.handle_restrictions(node=node),
                    {"enumeration": ["initial", "tracking"]},
                )

    def test_node_extractor(self):
        self.maxDiff = None
        with tempfile.NamedTemporaryFile(delete_on_close=False) as tmp:
            content = b"""
            <xs:schema xmlns:om="http://openmalaria.org/schema/scenario_47" xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://openmalaria.org/schema/scenario_47">
                <xs:element name="scenario">
                    <xs:annotation>
                        <xs:documentation>Description of scenario</xs:documentation>
                        <xs:appinfo>name:Scenario;</xs:appinfo>
                    </xs:annotation>
                    <xs:complexType>
                        <xs:all>
                            <xs:element name="demography" type="om:Demography">
                                <xs:annotation>
                                    <xs:documentation>
                                    Description of demography
                                    </xs:documentation>
                                    <xs:appinfo>name:Human age distribution;</xs:appinfo>
                                </xs:annotation>
                            </xs:element>
                            <xs:element name="monitoring" type="om:Monitoring">
                                <xs:annotation>
                                    <xs:documentation>
                                    Description of surveys
                                    </xs:documentation>
                                    <xs:appinfo>name:Measures to be reported;</xs:appinfo>
                                </xs:annotation>
                            </xs:element>
                        </xs:all>
                    </xs:complexType>
                </xs:element>
            </xs:schema>
            """
            tmp.write(content)
            tmp.close()

            xsd_init = XSDparser(47, str(tmp.name))
            self.assertEqual(
                xsd_init.node_extractor(
                    xsd_init.xml_root.iter(f"{xsd_init.xs_ns}element"),
                    {},
                    xsd_init.namespaces,
                    True,
                ),
                {
                    (0, "scenario"): {
                        "attributes": {},
                        "children": {
                            "demography": "om:Demography",
                            "monitoring": "om:Monitoring",
                        },
                        "docs": "\nDescription of scenario\nname:\tScenario",
                        "info": {
                            "children_metadata": {
                                "order": ["demography", "monitoring"],
                                "ordered": "all",
                            }
                        },
                    },
                    (1, "demography"): {
                        "attributes": {},
                        "children": {},
                        "docs": "\n"
                        "Description of demography\n"
                        "\n"
                        "name:\tHuman age distribution",
                        "info": {
                            "children_metadata": {"order": [], "ordered": None},
                            "type": "om:Demography",
                        },
                    },
                    (2, "monitoring"): {
                        "attributes": {},
                        "children": {},
                        "docs": "\n"
                        "Description of surveys\n"
                        "\n"
                        "name:\tMeasures to be reported",
                        "info": {
                            "children_metadata": {"order": [], "ordered": None},
                            "type": "om:Monitoring",
                        },
                    },
                },
            )

    def test_merge_dicts(self):
        with tempfile.NamedTemporaryFile(delete_on_close=False) as tmp:
            content = b"""
            <xs:schema xmlns:om="http://openmalaria.org/schema/scenario_47" xmlns:xs="http://www.w3.org/2001/XMLSchema" targetNamespace="http://openmalaria.org/schema/scenario_47">
                <xs:element name="demography" type="om:Demography">
                    <xs:annotation>
                        <xs:documentation>
                        Description of demography
                        </xs:documentation>
                        <xs:appinfo>name:Human age distribution;</xs:appinfo>
                    </xs:annotation>
                </xs:element>
                <xs:complexType name="Demography">
                    <xs:sequence>
                        <xs:element name="ageGroup" type="om:DemogAgeGroup">
                            <xs:annotation>
                            <xs:documentation>
                                list of age groups included in demography
                            </xs:documentation>
                            <xs:appinfo>name:Age groups;</xs:appinfo>
                            </xs:annotation>
                        </xs:element>
                    </xs:sequence>
                </xs:complexType>
            </xs:schema>
            """
            tmp.write(content)
            tmp.close()

            xsd_init = XSDparser(47, str(tmp.name))
            self.assertEqual(
                xsd_init.node_extractor(
                    xsd_init.xml_root.iter(f"{xsd_init.xs_ns}element"),
                    {},
                    xsd_init.namespaces,
                    True,
                ),
                {
                    (0, "scenario"): {
                        "attributes": {},
                        "children": {
                            "demography": "om:Demography",
                            "monitoring": "om:Monitoring",
                        },
                        "docs": "\nDescription of scenario\nname:\tScenario",
                        "info": {
                            "children_metadata": {
                                "order": ["demography", "monitoring"],
                                "ordered": "all",
                            }
                        },
                    },
                    (1, "demography"): {
                        "attributes": {},
                        "children": {},
                        "docs": "\n"
                        "Description of demography\n"
                        "\n"
                        "name:\tHuman age distribution",
                        "info": {
                            "children_metadata": {"order": [], "ordered": None},
                            "type": "om:Demography",
                        },
                    },
                    (2, "monitoring"): {
                        "attributes": {},
                        "children": {},
                        "docs": "\n"
                        "Description of surveys\n"
                        "\n"
                        "name:\tMeasures to be reported",
                        "info": {
                            "children_metadata": {"order": [], "ordered": None},
                            "type": "om:Monitoring",
                        },
                    },
                },
            )

if __name__ == "__main__":
    unittest.main()
