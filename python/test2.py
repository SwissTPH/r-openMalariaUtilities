#!/usr/bin/env python3

import lxml.etree as etree
# from pprint import pprint
from om_44_classes import OM0Scenario

f = open('om_44_classes.py')
source = f.read()
exec(source)

xml_tree = etree.ElementTree(etree.XML('''\
<?xml version="1.0" encoding="UTF-8" standalone="no"?>\
<om:scenario xmlns:om="http://openmalaria.org/schema/scenario_44" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://openmalaria.org/schema/scenario_44 scenario_44.xsd">\
</om:scenario>
'''.encode('utf-8')))

foo_root = xml_tree.getroot()

namespaces = {'om': 'http://openmalaria.org/schema/scenario_44',
              'xs': 'http://www.w3.org/2001/XMLSchema'}

# foo_root.findall("./om:scenario", namespaces=namespaces)
# print(etree.tostring(foo_root, pretty_print=True))

# print(foo_root.tag)
# etree.SubElement(foo_root, "demography")
# print(foo_root.tag)
# # etree.SubElement(foo_root, "demography")

# print(etree.tostring(foo_root, pretty_print=True))

foo = OM0Scenario(xml_tree)
# foo.demography.add()
foo.demography.name.set("FOOOOOOOOO")
# foo.demography.add()
foo.monitoring.surveys.surveyTime.add()
foo.monitoring.surveys.surveyTime.set("1t")
foo.monitoring.surveys.surveyTime.add()
foo.monitoring.surveys.surveyTime.set("2t")
foo.monitoring.surveys.surveyTime.set("3t")
foo.monitoring.surveys.surveyTime.repeatStep.set("10d")
foo.monitoring.surveys.surveyTime.repeatEnd.set("11d")
foo.monitoring.surveys.surveyTime.add()
foo.monitoring.surveys.surveyTime.set("4t")
foo.monitoring.surveys.surveyTime.repeatStep.set("10d")
foo.monitoring.surveys.surveyTime.repeatEnd.set("12d")

print(etree.tostring(foo.xml_tree, pretty_print=True).decode())

foo.monitoring.surveys.surveyTime.select(repeatStep = "10d")
foo.monitoring.surveys.surveyTime.select(repeatStep = "10d", repeatEnd="12d")
foo.monitoring.surveys.surveyTime.set("8t")
foo.monitoring.surveys.surveyTime.repeatStep.set("15d")
foo.monitoring.surveys.surveyTime.repeatEnd.set("20d")

print(etree.tostring(foo.xml_tree, pretty_print=True).decode())

foo.monitoring.surveys.surveyTime.update("3t", "5t", repeatStep="10d")

foo.monitoring.from_xml("""<monitoring>Child 4</monitoring>""")
foo.help()

foo.view()
# foo.demography.ageGroup.group.add()
# foo.demography.ageGroup.group.upperbound.set(4)
# foo.demography.ageGroup.group.poppercent.set(3.49879879)
# foo.demography.ageGroup.group.upperbound.set(5)
# foo.demography.ageGroup.group.poppercent.set(8.7665)
# foo.demography.ageGroup.group.add()
# foo.demography.ageGroup.group.upperbound.set(4)
# foo.demography.ageGroup.group.poppercent.set(10.836323739)
# foo.demography.ageGroup.group.upperbound.set(20)
# foo.demography.ageGroup.group.poppercent.set(4.182561874)
# # foo.demography.help()
# print(etree.tostring(foo.xml_tree, pretty_print=True).decode())

# foo.demography.ageGroup.lowerbound.doc
# # etree.tostring(foo.demography._xml_tree, pretty_print=True).decode()

# foo.demography.ageGroup.group.poppercent.set()

# test = etree.XML("""<root>
# <child foo="1">Child 1</child>
#   <child foo="2">Child 2</child>
#   <another>Child 3</another>
# </root>""")

# # test.insert(0, etree.XML("""<child>Child 4</child>\n<child>Child 5</child>"""))
# # len(test)
# # test.replace()
# found = test.findall(".//child[@foo='2'][text()='Child 2']")
# test.replace(found[0], etree.XML("""<a bar='1'/>"""))
# # # print(etree.tostring(found[0]))
# print(etree.tostring(test))
