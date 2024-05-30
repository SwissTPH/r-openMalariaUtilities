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
    """Extracts detailed information from XML schema elements and organizes it
    into a dictionary.

    This function iterates over a collection of XML schema elements, extracting
    documentation, attributes, and child elements, and organizes this
    information into a nested dictionary structure.

    Parameters
    ----------
    nodes (etree.Element): The XML schema elements to process.
    data_dict (dict): The dictionary to populate with extracted information.
    namespaces (dict[str, str]): A dictionary mapping namespace prefixes to
      their URIs.
    is_top_level (bool, optional): A flag indicating if the nodes are top-level
      elements. Default is False.

    Returns
    -------
    dict: A dictionary containing detailed information about the XML schema
      elements.

    """
    # Initialize a counter for top-level elements
    counter = 0

    # Iterate over all provided XML schema elements
    for node in nodes:
        # Get the 'name' attribute of the current node
        node_name = node.attrib.get("name")
        if node_name is None:
            continue
        else:
            # Determine the key based on whether the node is top-level or not
            if is_top_level:
                key = (counter, node_name)
                counter += 1
            else:
                key = node_name

            # Populate the dictionary with extracted documentation, attributes,
            # and children
            data_dict[key] = {
                # Extract documentation text using docs_extractor function
                "docs": docs_extractor(node, namespaces),

                # Extract attributes using attr_extractor function
                "info": attr_extractor(node),

                # Recursively extract attributes from nested attribute elements
                "attributes": node_extractor(
                    # Find all direct attribute elements
                    node.findall(f"./{xs_ns}attribute")
                    # Find attribute elements within complexType
                    + node.findall(f"./{xs_ns}complexType/{xs_ns}attribute"),
                    # Initialize an empty dictionary for nested attributes
                    {},
                    # Pass the namespaces dictionary
                    namespaces,
                ),

                # Initialize an empty dictionary for children elements
                "children": {},
            }

            # Add metadata for children elements, indicating whether they are
            # ordered and their order
            data_dict[key]["info"]["children_metadata"] = {"ordered": None, "order": []}

            # Find attribute restrictions
            # Check if the current node is an attribute element
            if node.tag == xs_ns + "attribute":
                # Find the restriction node within the simpleType of the
                # attribute
                restriction_node = node.find(f"./{xs_ns}simpleType/{xs_ns}restriction")

                # If a restriction node is found, handle the restrictions
                if restriction_node is not None:
                    # Extract restrictions using the handle_restrictions
                    # function
                    restrictions = handle_restrictions(restriction_node)

                    # Add the extracted restrictions to the info dictionary
                    # under the 'restrictions' key
                    data_dict[key]["info"]["restrictions"] = restrictions

                    # Add the base type of the restriction to the info
                    # dictionary under the 'type' key
                    data_dict[key]["info"]["type"] = restriction_node.attrib.get("base")

            # Handle xs:extension
            # Try to find an extension node within complexType/complexContent or
            # complexType/simpleContent
            extension_node = node.find(
                f"{xs_ns}complexType/{xs_ns}complexContent/{xs_ns}extension"
            ) or node.find(
                f"./{xs_ns}complexContent/{xs_ns}extension"
            ) or node.find(
                f"{xs_ns}complexType/{xs_ns}simpleContent/{xs_ns}extension"
            ) or node.find(f"./{xs_ns}simpleContent/{xs_ns}extension")

            # If an extension node is found, process it
            if extension_node is not None:
                # Get the base type of the extension
                base_type = extension_node.attrib.get("base")

                # Add the base type to the data dictionary under the
                # 'extension_base' key
                data_dict[key]["extension_base"] = base_type

                # Process attributes and elements inside the extension by
                # recursively calling node_extractor
                data_dict[key]["attributes"].update(
                    node_extractor(extension_node, {}, namespaces)
                )

                # If the current node is an element and does not have a type
                # attribute, set its type to the base type
                if node.tag == xs_ns + "element" and node.attrib.get("type", None) is None:
                    data_dict[key]["info"]["type"] = base_type

            # Find children
            # Check for unordered children by looking for an 'all' element
            # within complexType or directly under the node
            if (candidates := node.find("./xs:complexType/xs:all", namespaces=namespaces) or node.find("./xs:all", namespaces=namespaces)) is not None:
                kids = candidates
                data_dict[key]["info"]["children_metadata"]["ordered"] = "all"

                # Check for ordered children by looking for a 'sequence' element
                # within complexType or directly under the node
            elif (candidates := node.find("./xs:complexType/xs:sequence", namespaces=namespaces) or node.find("./xs:sequence", namespaces=namespaces)) is not None:
                kids = candidates
                data_dict[key]["info"]["children_metadata"]["ordered"] = "sequence"

                # If no 'all' or 'sequence' elements are found, consider the
                # current node as having children
            else:
                kids = node

            # Process children elements
            # Iterate over all child elements
            for kid in kids:
                # Check if the child is an 'element'
                if kid.tag == xs_ns + "element":
                    # Get the 'name' attribute of the child element
                    child_name = kid.attrib.get("name")

                    # Get the 'type' attribute of the child element
                    child_type = kid.attrib.get("type")

                    # If there is no 'type' attribute, look for an extension of
                    # this element
                    if child_type is None:
                        extension_node = kid.find("./xs:complexType/xs:complexContent/xs:extension", namespaces=namespaces)
                        if extension_node is not None:
                            # Get the base type from the extension node
                            child_type = extension_node.attrib.get("base")

                    # Add the child element's name and type to the children
                    # dictionary
                    data_dict[key]["children"][child_name] = child_type

                    # Append the child's name to the children metadata order
                    # list
                    data_dict[key]["info"]["children_metadata"]["order"].append(child_name)

                # Check if the child is a 'choice' element
                elif kid.tag == xs_ns + "choice":
                    choices = []
                    # Iterate over all elements within the 'choice'
                    for choice in kid:
                        if choice.tag == xs_ns + "element":
                            # Get the 'name' and 'type' attributes of the choice
                            # element
                            child_name = choice.attrib.get("name")
                            child_type = choice.attrib.get("type")

                            # Add the choice element's name and type to the
                            # children dictionary
                            data_dict[key]["children"][child_name] = child_type

                            # Append the choice element's name to the choices
                            # list
                            choices.append(child_name)
                        elif choice.tag == xs_ns + "sequence":
                            sequence_choices = []
                            # Iterate over all elements within the 'sequence'
                            for seq_element in choice:
                                if seq_element.tag == xs_ns + "element":
                                    # Get the 'name' and 'type' attributes of
                                    # the sequence element
                                    seq_child_name = seq_element.attrib.get("name")
                                    seq_child_type = seq_element.attrib.get("type")

                                    # Add the sequence element's name and type
                                    # to the children dictionary
                                    data_dict[key]["children"][seq_child_name] = seq_child_type

                                    # Append the sequence element's name to the
                                    # sequence choices list
                                    sequence_choices.append(seq_child_name)
                            # Append the sequence choices list to the choices
                            # list
                            choices.append(sequence_choices)

                    # Append the choices list to the children metadata order
                    # list
                    data_dict[key]["info"]["children_metadata"]["order"].append(choices)

    # Return the updated data dictionary
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
