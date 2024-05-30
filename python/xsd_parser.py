#!/usr/bin/env python3

from lxml import etree
from pprint import pprint
import re

parser = etree.XMLParser(remove_comments=True)

tree = etree.parse("../example.xsd", parser=parser)
tree = etree.parse("../scenario_44.xsd", parser=parser)
root = tree.getroot()

namespaces = {
    "om": "http://openmalaria.org/schema/scenario_44",
    "xs": "http://www.w3.org/2001/XMLSchema",
}

xs_ns = "{http://www.w3.org/2001/XMLSchema}"


def docs_extractor(node: etree.Element, namespaces: dict[str, str]) -> str:
    """Extracts and combines documentation and appinfo text from an XML schema
    element.

    This function locates the `xs:documentation` and `xs:appinfo` nodes within
    the `xs:annotation` of the provided XML schema element, cleans and formats
    their text content, and combines them into a single string.

    Parameters
    ----------
    node (etree.Element): The XML schema element from which to extract
      documentation and appinfo.

    namespaces (dict[str, str]): A dictionary mapping namespace prefixes to
      their URIs.

    Returns
    -------
    str: A combined string of cleaned documentation and formatted appinfo text,
      or an empty string if none is found.
    """
    # Find the documentation node within the annotation of the given XML schema
    # element
    doc_node = node.find("./xs:annotation/xs:documentation", namespaces=namespaces)
    # Extract the text from the documentation node, if it exists and is not
    # empty, and strip leading whitespace
    doc_text = doc_node.text.lstrip() if doc_node is not None and doc_node.text else None
    # Clean up the documentation text by removing excessive whitespace and
    # ensuring each line starts from the left
    doc_text = "\n" + re.sub("\n\s+", "\n", doc_text) if doc_text is not None else None
    # Find the appinfo node within the annotation of the given XML schema
    # element
    appinfo_node = node.find("./xs:annotation/xs:appinfo", namespaces=namespaces)
    # Extract the text from the appinfo node, replace semicolons with
    # newlines, remove leading/trailing whitespace, and replace colons with
    # tabs
    if appinfo_node is not None and appinfo_node.text:
        appinfo_text = appinfo_node.text.replace(";", "\n").strip().replace(":", ":\t")
    else:
        appinfo_text = None

    # Combine the cleaned documentation text and appinfo text, filtering out any
    # None values
    combined_text = "\n".join(filter(None, [doc_text, appinfo_text]))

    return combined_text


def attr_extractor(node: etree.Element) -> dict:
    """Extracts all attributes from an XML element except the 'name' attribute.

    This function iterates over all attributes of the provided XML element and
    extracts them into a dictionary, excluding the 'name' attribute.

    Parameters
    ----------
    node (etree.Element): The XML element from which to extract attributes.

    Returns
    -------
    dict: A dictionary containing all attributes of the XML element, except the
      'name' attribute.
    """
    # Initialize an empty dictionary to store attribute information
    attribute_info = {}

    # Iterate over all attributes of the given XML element
    for attr in node.attrib:
        # Exclude the 'name' attribute since it's already used as a key
        if attr != "name":
            # Add the attribute to the dictionary
            attribute_info[attr] = node.attrib[attr]

    # Return the dictionary containing the attributes
    return attribute_info

def handle_restrictions(node: etree.Element) -> dict:
    """Extracts enumeration restrictions from an XML schema element.

    This function iterates over the child elements of the provided XML schema
    element and collects enumeration restrictions into a dictionary.

    Parameters
    ----------
    node (etree.Element): The XML schema element from which to extract
      enumeration restrictions.

    Returns
    -------
    dict: A dictionary containing enumeration restrictions, with 'enumeration'
      as the key and a list of values as the value.
    """
    # Initialize an empty dictionary to store restrictions
    restrictions = {}

    # Iterate over all child elements of the given XML schema element
    for child in node:
        # Check if the child's tag is 'enumeration'
        if child.tag == xs_ns + 'enumeration':
            # Add the value of the 'enumeration' attribute to the restrictions
            # dictionary
            restrictions.setdefault('enumeration', []).append(child.attrib.get('value'))

    # Return the dictionary containing the restrictions
    return restrictions

def node_extractor(nodes: etree.Element, data_dict: dict, namespaces: dict[str, str], is_top_level: bool = False) -> dict:
    counter = 0
    for node in nodes:
        node_name = node.attrib.get("name")
        if node_name is None:
            continue
        else:
            if is_top_level:
                key = (counter, node_name)
                counter += 1
            else:
                key = node_name

            data_dict[key] = {
                "docs": docs_extractor(node, namespaces),
                "info": attr_extractor(node),
                "attributes": node_extractor(
                    node.findall(f"./{xs_ns}attribute")
                    + node.findall(f"./{xs_ns}complexType/{xs_ns}attribute"),
                    {},
                    namespaces,
                ),
                "children": {},
            }
            data_dict[key]["info"]["children_metadata"] = {"ordered": None, "order": []}

            # Find attribute restrictions
            if node.tag == xs_ns + "attribute":
                restriction_node = node.find(f"./{xs_ns}simpleType/{xs_ns}restriction")
                if restriction_node is not None:
                    restrictions = handle_restrictions(restriction_node)
                    data_dict[key]["info"]["restrictions"] = restrictions
                    data_dict[key]["info"]["type"] = restriction_node.attrib.get("base")


            # Handle xs:extension
            extension_node = node.find(
                f"{xs_ns}complexType/{xs_ns}complexContent/{xs_ns}extension"
            ) or node.find(
                f"./{xs_ns}complexContent/{xs_ns}extension"
            ) or node.find(
                f"{xs_ns}complexType/{xs_ns}simpleContent/{xs_ns}extension"
            ) or node.find(f"./{xs_ns}simpleContent/{xs_ns}extension")
            if extension_node is not None:
                base_type = extension_node.attrib.get("base")
                data_dict[key]["extension_base"] = base_type
                # Process attributes and elements inside the extension
                # pprint(node_extractor(extension_node, {}, namespaces))
                data_dict[key]["attributes"].update(
                    node_extractor(extension_node, {}, namespaces)
                )
                if node.tag == xs_ns + "element" and node.attrib.get("type", None) is None:
                    data_dict[key]["info"]["type"] = base_type

            # Find children
            # Unordered
            if (candidates := node.find("./xs:complexType/xs:all", namespaces=namespaces) or node.find("./xs:all", namespaces=namespaces)) is not None:
                kids = candidates
                data_dict[key]["info"]["children_metadata"]["ordered"] = "all"
            # Ordered
            elif (candidates := node.find("./xs:complexType/xs:sequence", namespaces=namespaces) or node.find("./xs:sequence", namespaces=namespaces)) is not None:
                kids = candidates
                data_dict[key]["info"]["children_metadata"]["ordered"] = "sequence"
            else:
                kids = node

            for kid in kids:
                if kid.tag == xs_ns + "element":
                    child_name = kid.attrib.get("name")
                    child_type = kid.attrib.get("type")
                    # If there is no type, look if there is an extension of this element
                    if child_type is None:
                        extension_node = kid.find("./xs:complexType/xs:complexContent/xs:extension", namespaces=namespaces)
                        if extension_node is not None:
                            child_type = extension_node.attrib.get("base")

                    # print(child_name, "is a child of", node_name)
                    data_dict[key]["children"][child_name] = child_type
                    data_dict[key]["info"]["children_metadata"]["order"].append(child_name)

                elif kid.tag == xs_ns + "choice":
                    choices = []
                    for choice in kid:
                        if choice.tag == xs_ns + "element":
                            child_name = choice.attrib.get("name")
                            child_type = choice.attrib.get("type")
                            data_dict[key]["children"][child_name] = child_type
                            choices.append(child_name)
                        elif choice.tag == xs_ns + "sequence":
                            sequence_choices = []
                            for seq_element in choice:
                                if seq_element.tag == xs_ns + "element":
                                    seq_child_name = seq_element.attrib.get("name")
                                    seq_child_type = seq_element.attrib.get("type")
                                    data_dict[key]["children"][seq_child_name] = seq_child_type
                                    sequence_choices.append(seq_child_name)
                            choices.append(sequence_choices)

                    data_dict[key]["info"]["children_metadata"]["order"].append(choices)

    return data_dict


element_data = node_extractor(root.iter(f"{xs_ns}element"), {}, namespaces, True)
complex_data = node_extractor(root.iter(f"{xs_ns}complexType"), {}, namespaces, True)

# pprint(element_data)
# pprint(complex_data)

# for key in element_data.keys():
#     if "lifeCycle" in key:
#         pprint(element_data[key])

# for key in complex_data.keys():
#     if "Demography" in key:
#         pprint(complex_data[key])

# for key in element_data.keys():
#     if "larvalStage" in key:
#         pprint(element_data[key])

# for key in element_data.keys():
#     if "continuous" in key:
#         print(key)
#         pprint(element_data[key])

# for key in complex_data.keys():
#     if "TreatmentOption" in key:
#         pprint(complex_data[key])

# # for key in element_data.keys():
# #     if "clearInfections" in key:
# #         pprint(element_data[key])

# for key in complex_data.keys():
#     if "OptionSet" in key:
#         print(key)
#         pprint(complex_data[key])

# for key in element_data.keys():
#     if "surveyTime" in key:
#         print(key)
#         pprint(element_data[key])

# for key in complex_data.keys():
#     if "Option" in key:
#         print(key)
#         pprint(complex_data[key])

# for key in element_data.keys():
#     if "option" in key:
#         print(key)
#         pprint(element_data[key])

# for key in element_data.keys():
#     if "continuous" in key:
#         print(key)
#         pprint(element_data[key])

# for key in merged_element_data.keys():
#     if "continuous" in key:
#         print(key)
#         pprint(merged_element_data[key])

# for key in complex_data.keys():
#     if "Monitoring" in key:
#         print(key)
#         pprint(complex_data[key])

# for key in merged_element_data.keys():
#     if "monitoring" in key:
#         print(key)
#         pprint(merged_element_data[key])

# for key in element_data.keys():
#     if "secondReleaseDays" in key:
#         print(key)
#         pprint(element_data[key])

# for key in complex_data.keys():
#     if "HypnozoiteReleaseDistribution" in key:
#         print(key)
#         pprint(complex_data[key])

# for key in complex_data.keys():
#     if "SampledValueLN" in key:
#         print(key)
#         pprint(complex_data[key])

# for key in complex_data.keys():
#     if "SampledValueCV" in key:
#         print(key)
#         pprint(complex_data[key])

# for key in merged_element_data.keys():
#     if "surveyTime" in key:
#         print(key)
#         pprint(merged_element_data[key])

# for key in complex_data.keys():
#     if "EIRDaily" in key:
#         print(key)
#         pprint(complex_data[key])

# for key in element_data.keys():
#     if "EIRDaily" in key:
#         print(key)
#         pprint(element_data[key])

# for key in merged_element_data.keys():
#     if "EIRDaily" in key:
#         print(key)
#         pprint(merged_element_data[key])

# for key in element_data.keys():
#     if "PK" in key:
#         print(key)
#         pprint(element_data[key])

# for key, value in merged_element_data.items():
#     if value["info"].get("type", "").startswith("xs:"):
#         print(key)
