#!/usr/bin/env python3

import importlib

import lxml.etree as etree


class OMExperiment:
    """Prototype super-class for OM classes."""

    def __init__(self, om_version):
        om_version = str(om_version)
        om_module_name = f"om_{om_version}_classes"
        om_classes = importlib.import_module(om_module_name)
        xml_string = (
            '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'
            + f'<om:scenario xmlns:om="http://openmalaria.org/schema/scenario_{om_version}" '
            + 'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" '
            + f'xsi:schemaLocation="http://openmalaria.org/schema/scenario_{om_version} '
            + f'scenario_{om_version}.xsd">'
            + "</om:scenario>"
        )
        self.xml_tree = etree.ElementTree(
            etree.XML(bytes(xml_string, encoding="utf-8"))
        )
        self.scenario = om_classes.OM0Scenario(self.xml_tree)
        self.scenario_configs = []

    def write_xml(self, file):
        """Write stored xml to file."""
        self.xml_tree.write(file, pretty_print=True)
