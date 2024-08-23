#!/usr/bin/env python3

"""A parser/generator for OpenMalaria XML schema definitions."""

import os
import re
from collections.abc import Iterable

from lxml import etree


class XSDparser:
    """A class used to represent a parsed OpenMalaria XML schema definition.

    This class contains the information found in the XSD file and provides
    methods to generate classes from the found information and write them to a
    file.

    Parameters
    ----------
    om_version : int or str
      The targeted OpenMalaria schema version.

    input_file : str or os.PathLike
      Path to the file to parse.

    Attributes
    ----------
    om_version : str
      Stores the targeted OpenMalaria schema version.

    xsd_source : str or os.PathLike
      Stores the XSD input file.

    namespaces : dict
      Dictionary containing the XML namespaces.

    xs_ns : str
      Stores the standard ``xs`` namespace.

    parser : etree.XMLParser
      The parser used.

    xml_tree : etree.ElementTree
      The parsed XML tree.

    xml_root : etree.Element
      The root of the parsed XML.

    element_data : dict
      Dictionary representing the parsed element data.

    complex_data : dict
      Dictionary representing the parsed complexType data.

    """

    def __init__(self, om_version: int | str, input_file: str | os.PathLike) -> None:
        self.om_version = str(om_version)
        self.xsd_source = input_file
        self.namespaces: dict = {
            "om": "http://openmalaria.org/schema/scenario_" + self.om_version,
            "xs": "http://www.w3.org/2001/XMLSchema",
        }
        self.xs_ns: str = "{http://www.w3.org/2001/XMLSchema}"
        self.parser: etree.XMLParser = etree.XMLParser(remove_comments=True)
        self.xml_tree: etree._ElementTree = etree.parse(
            self.xsd_source, parser=self.parser
        )
        self.xml_root: etree._Element = self.xml_tree.getroot()
        self.element_data: dict = self.node_extractor(
            self.xml_root.iter(f"{self.xs_ns}element"), {}, self.namespaces, True
        )
        self.complex_data: dict = self.node_extractor(
            self.xml_root.iter(f"{self.xs_ns}complexType"), {}, self.namespaces, True
        )

    def check_handled_tags(self):
        """Check if this parser handles all found tags."""
        # Collect all tags
        all_tags = {element.tag for element in self.xml_root.iter()}

        # Remove the namespace for better readability:
        cleaned_tags = {tag.split("}")[-1] for tag in all_tags}
        handled_tags = {
            "all",
            "annotation",
            "appinfo",
            "attribute",
            "choice",
            # So far only appears with extension, could also be together with
            # restriction
            "complexContent",
            "complexType",
            "documentation",
            "element",
            # Enumeration is so far always together with restriction in an attribute.
            # Let's hope it stays like that
            "enumeration",
            "extension",
            # This is an artifact from a html tag
            "i",
            # See enumeration
            "restriction",
            # Does not need any processing
            "schema",
            "sequence",
            # These two are handled implicitly:
            # Appears together with xs:extension
            "simpleContent",
            # Appears together with xs:restriction
            "simpleType",
        }
        unhandled_tags = {x for x in cleaned_tags if x not in handled_tags}
        if len(unhandled_tags) > 0:
            print(f"Warning: Found unhandled tags: {unhandled_tags!r}")
            for tag in unhandled_tags:
                xpath_elements = self.xml_root.findall(
                    f".//xs:{tag}", namespaces=self.namespaces
                )
                if xpath_elements:
                    print(
                        "The pattern",
                        f".//xs:{tag}",
                        "appears",
                        len(xpath_elements),
                        "times in the XSD.",
                    )
                    for x_el in xpath_elements:
                        ele = self.xml_tree.find(self.xml_tree.getelementpath(x_el))
                        if ele is not None:
                            print(etree.tostring(ele, pretty_print=True).decode())
                else:
                    print("The pattern does not appear in the XSD.")
        else:
            print("All tags handled!")

    def generate_classes(self, output_file: str | os.PathLike) -> None:
        """Generate OpenMalaria classes and write them to output file.

        Parameters
        ----------
        output_file : str or os.PathLike
          The file to write the classes to.

        """
        # Subsequently merge data to element_data and prepare output
        merged_element_data = self.merged_elements(self.element_data, self.complex_data)
        merged_element_data = self.merge_extensions(
            merged_element_data, self.complex_data
        )
        merged_element_data = self.finalize_metadata(merged_element_data)

        # Generate Python class code
        header = []
        header.append(
            f'"""Provides autogenerated classes for OpenMalaria XML schema V{str(self.om_version)}.'
        )
        header.append("")
        header.append("Manual changes will be overwritten!.")
        header.append('"""')
        header.append("")
        header.append("from nodes import AttributeNode, ElementNode, Node")
        header.append("from lxml import etree")
        header.append("")
        header_code = "\n".join(header)

        class_code_lines = self.generate_class_code(
            merged_element_data, self.complex_data
        )
        class_code = "\n".join(class_code_lines)
        with open(output_file, "w") as f:
            f.write(header_code)
            f.write("\n")
            f.write(class_code)

    def docs_extractor(self, node: etree._Element, namespaces: dict[str, str]) -> str:
        """Extract and combine documentation and appinfo text from XML schema elements.

        This function locates the ``xs:documentation`` and ``xs:appinfo`` nodes within
        the ``xs:annotation`` of the provided XML schema element, cleans and formats
        their text content, and combines them into a single string.

        Parameters
        ----------
        node : etree.Element
          The XML schema element from which to extract documentation and appinfo.

        namespaces : dict[str, str]
            A dictionary mapping namespace prefixes to their URIs.

        Returns
        -------
        str :
          A combined string of cleaned documentation and formatted appinfo text, or an
          empty string if none is found.

        """
        # Find the documentation node within the annotation of the given XML
        # schema element
        doc_node = node.find("./xs:annotation/xs:documentation", namespaces=namespaces)
        # Extract the text from the documentation node, if it exists and is
        # not empty, and strip leading whitespace
        doc_text = (
            doc_node.text.lstrip() if doc_node is not None and doc_node.text else None
        )

        # Clean up the documentation text by removing excessive whitespace
        # and ensuring each line starts from the left
        doc_text = (
            "\n" + re.sub("\n\\s+", "\n", doc_text) if doc_text is not None else None
        )
        # Find the appinfo node within the annotation of the given XML
        # schema element
        appinfo_node = node.find("./xs:annotation/xs:appinfo", namespaces=namespaces)
        # Extract the text from the appinfo node, replace semicolons with
        # newlines, remove leading/trailing whitespace, and replace colons
        # with tabs
        if appinfo_node is not None and appinfo_node.text:
            appinfo_text = (
                appinfo_node.text.replace(";", "\n").strip().replace(":", ":\t")
            )
        else:
            appinfo_text = None

        # Combine the cleaned documentation text and appinfo text, filtering
        # out any None values
        combined_text = "\n".join(filter(None, [doc_text, appinfo_text]))

        return combined_text

    def attr_extractor(self, node: etree._Element) -> dict:
        """Extract all attributes from an XML element except the 'name' attribute.

        This function iterates over all attributes of the provided XML element and
        extracts them into a dictionary, excluding the 'name' attribute.

        Parameters
        ----------
        node : etree.Element
          The XML element from which to extract attributes.

        Returns
        -------
        dict :
          A dictionary containing all attributes of the XML element, except the 'name'
          attribute.

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

    def handle_restrictions(self, node: etree._Element) -> dict:
        """Extract enumeration restrictions from an XML schema element.

        This function iterates over the child elements of the provided XML schema
        element and collects enumeration restrictions into a dictionary.

        Parameters
        ----------
        node : etree.Element
          The XML schema element from which to extract enumeration restrictions.

        Returns
        -------
        dict :
          A dictionary containing enumeration restrictions, with 'enumeration' as the
          key and a list of values as the value.

        """
        # Initialize an empty dictionary to store restrictions
        restrictions = {}

        # Iterate over all child elements of the given XML schema element
        for child in node:
            # Check if the child's tag is 'enumeration'
            if child.tag == self.xs_ns + "enumeration":
                # Add the value of the 'enumeration' attribute to the restrictions
                # dictionary
                restrictions.setdefault("enumeration", []).append(
                    child.attrib.get("value")
                )

        # Return the dictionary containing the restrictions
        return restrictions

    def node_extractor(
        self,
        nodes: Iterable[etree._Element],
        data_dict: dict,
        namespaces: dict[str, str],
        is_top_level: bool = False,
    ) -> dict:
        """Extract detailed information from XML schema elements into a dictionary.

        This function iterates over a collection of XML schema elements, extracting
        documentation, attributes, and child elements, and organizes this information
        into a nested dictionary structure.

        Parameters
        ----------
        nodes : etree.Element
          The XML schema elements to process.

        data_dict :
          The dictionary to populate with extracted information.

        namespaces : dict[str, str]
          A dictionary mapping namespace prefixes to their URIs.

        is_top_level : bool, default False
          A flag indicating if the nodes are top-level elements.

        Returns
        -------
        dict :
          A dictionary containing detailed information about the XML schema elements.

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

                # Populate the dictionary with extracted documentation,
                # attributes, and children
                data_dict[key] = {
                    # Extract documentation text using docs_extractor function
                    "docs": self.docs_extractor(node, namespaces),
                    # Extract attributes using attr_extractor function
                    "info": self.attr_extractor(node),
                    # Recursively extract attributes from nested attribute
                    # elements
                    "attributes": self.node_extractor(
                        # Find all direct attribute elements
                        node.findall(f"./{self.xs_ns}attribute")
                        # Find attribute elements within complexType
                        + node.findall(
                            f"./{self.xs_ns}complexType/{self.xs_ns}attribute"
                        ),
                        # Initialize an empty dictionary for nested attributes
                        {},
                        # Pass the namespaces dictionary
                        namespaces,
                    ),
                    # Initialize an empty dictionary for children elements
                    "children": {},
                }

                # Add metadata for children elements, indicating whether they
                # are ordered and their order
                data_dict[key]["info"]["children_metadata"] = {
                    "ordered": None,
                    "order": [],
                }

                # Find attribute restrictions
                # Check if the current node is an attribute element
                if node.tag == self.xs_ns + "attribute":
                    # Find the restriction node within the simpleType of the
                    # attribute
                    restriction_node = node.find(
                        f"./{self.xs_ns}simpleType/{self.xs_ns}restriction"
                    )

                    # If a restriction node is found, handle the restrictions
                    if restriction_node is not None:
                        # Extract restrictions using the handle_restrictions
                        # function
                        restrictions = self.handle_restrictions(restriction_node)

                        # Add the extracted restrictions to the info dictionary
                        # under the 'restrictions' key
                        data_dict[key]["info"]["restrictions"] = restrictions

                        # Add the base type of the restriction to the info
                        # dictionary under the 'type' key
                        data_dict[key]["info"]["type"] = restriction_node.attrib.get(
                            "base"
                        )

                # Handle xs:extension
                # Try to find an extension node within
                # complexType/complexContent or complexType/simpleContent
                extension_node = None

                if (
                    (
                        extension_node := node.find(
                            f"{self.xs_ns}complexType/{self.xs_ns}complexContent/{self.xs_ns}extension"
                        )
                    )
                    is not None
                    or (
                        extension_node := node.find(
                            f"./{self.xs_ns}complexContent/{self.xs_ns}extension"
                        )
                    )
                    is not None
                    or (
                        (
                            extension_node := node.find(
                                f"{self.xs_ns}complexType/{self.xs_ns}simpleContent/{self.xs_ns}extension"
                            )
                        )
                        is not None
                        or (
                            extension_node := node.find(
                                f"./{self.xs_ns}simpleContent/{self.xs_ns}extension"
                            )
                        )
                        is not None
                    )
                ):
                    pass

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
                        self.node_extractor(extension_node, {}, namespaces)
                    )

                    # If the current node is an element and does not have a type
                    # attribute, set its type to the base type
                    if (
                        node.tag == self.xs_ns + "element"
                        and node.attrib.get("type", None) is None
                    ):
                        data_dict[key]["info"]["type"] = base_type

                # Find children
                # Check for unordered children by looking for an 'all' element
                # within complexType or directly under the node
                if (
                    candidates := node.find(
                        "./xs:complexType/xs:all", namespaces=namespaces
                    )
                ) is not None or (
                    candidates := node.find("./xs:all", namespaces=namespaces)
                ) is not None:
                    kids = candidates
                    data_dict[key]["info"]["children_metadata"]["ordered"] = "all"

                    # Check for ordered children by looking for a 'sequence'
                    # element within complexType or directly under the node
                elif (
                    candidates := node.find(
                        "./xs:complexType/xs:sequence", namespaces=namespaces
                    )
                ) is not None or (
                    candidates := node.find("./xs:sequence", namespaces=namespaces)
                ) is not None:
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
                    if kid.tag == self.xs_ns + "element":
                        # Get the 'name' attribute of the child element
                        child_name = kid.attrib.get("name")

                        # Get the 'type' attribute of the child element
                        child_type = kid.attrib.get("type")

                        # If there is no 'type' attribute, look for an extension
                        # of this element
                        if child_type is None:
                            extension_node = kid.find(
                                "./xs:complexType/xs:complexContent/xs:extension",
                                namespaces=namespaces,
                            )
                            if extension_node is not None:
                                # Get the base type from the extension node
                                child_type = extension_node.attrib.get("base")

                        # Add the child element's name and type to the children
                        # dictionary
                        data_dict[key]["children"][child_name] = child_type

                        # Append the child's name to the children metadata order
                        # list
                        data_dict[key]["info"]["children_metadata"]["order"].append(
                            child_name
                        )

                    # Check if the child is a 'choice' element
                    elif kid.tag == self.xs_ns + "choice":
                        choices = []
                        # Iterate over all elements within the 'choice'
                        for choice in kid:
                            if choice.tag == self.xs_ns + "element":
                                # Get the 'name' and 'type' attributes of the
                                # choice element
                                child_name = choice.attrib.get("name")
                                child_type = choice.attrib.get("type")

                                # Add the choice element's name and type to the
                                # children dictionary
                                data_dict[key]["children"][child_name] = child_type

                                # Append the choice element's name to the
                                # choices list
                                choices.append(child_name)
                            elif choice.tag == self.xs_ns + "sequence":
                                sequence_choices = []
                                # Iterate over all elements within the
                                # 'sequence'
                                for seq_element in choice:
                                    if seq_element.tag == self.xs_ns + "element":
                                        # Get the 'name' and 'type' attributes
                                        # of the sequence element
                                        seq_child_name = seq_element.attrib.get("name")
                                        seq_child_type = seq_element.attrib.get("type")

                                        # Add the sequence element's name and
                                        # type to the children dictionary
                                        data_dict[key]["children"][seq_child_name] = (
                                            seq_child_type
                                        )

                                        # Append the sequence element's name to
                                        # the sequence choices list
                                        sequence_choices.append(seq_child_name)
                                # Append the sequence choices list to the
                                # choices list
                                choices.append(sequence_choices)

                        # Append the choices list to the children metadata order
                        # list
                        data_dict[key]["info"]["children_metadata"]["order"].append(
                            choices
                        )

        # Return the updated data dictionary
        return data_dict

    def merge_dicts(self, a: dict, b: dict) -> dict:
        """Recursively merge dictionary b into dictionary a.

        This function combines two dictionaries by recursively merging the values of
        common keys. Special handling is applied to keys named 'docs' and 'type' to
        concatenate their values.

        Parameters
        ----------
        a : dict
          The target dictionary to merge into.

        b : dict
          The source dictionary to merge from.

        Returns
        -------
        dict:
          A new dictionary with the merged content of a and b.

        """
        # Create a copy of dictionary a to avoid modifying the original
        target = a.copy()

        # Reference dictionary b as the source
        source = b

        # Iterate over all items in the source dictionary
        for key, value in source.items():
            # Check if the key exists in the target dictionary
            if key in target:
                # If the target key is empty, use the value from the source
                if not bool(target[key]):
                    target[key] = value
                # If the source value is empty, skip merging for this key
                elif not bool(value):
                    continue
                # If the key is 'docs' and both target and source have non-empty
                # values, concatenate them
                elif key == "docs" and bool(target[key]) and bool(value):
                    if bool(target.get("extension_base", "")):
                        target[key] = target[key] + "\nBase Type:" + value
                    else:
                        target[key] = "\nElement:" + target[key] + "\nType:" + value
                # If the key is another dictionary, recursively merge the
                # dictionaries
                elif isinstance(target[key], dict) and isinstance(value, dict):
                    target[key] = self.merge_dicts(target[key], value)
                # Otherwise, overwrite the target key with the source value
                else:
                    target[key] = value
            # If the key does not exist in the target, add it from the source
            else:
                target[key] = value

        # Return the merged dictionary
        return target

    def merge_extensions(self, element_data: dict, complex_data: dict) -> dict:
        """Merge extension data from complex types into element data.

        This function merges information from complex type extensions into the
        corresponding element data. It handles nested extensions and updates the base
        type of elements when needed.

        Parameters
        ----------
        element_data : dict
          The dictionary containing element data to be merged.

        complex_data : dict
          The dictionary containing complex type data to merge from.

        Returns
        -------
        dict :
          A dictionary with the merged element and complex type data.

        """
        # Create a copy of element_data to avoid modifying the original
        element_data_copy = element_data.copy()

        # Iterate over each item in the copied element data
        for key, value in element_data_copy.items():
            # Get the current element data
            cur_el = value

            # Get the extension base of the current element
            cur_el_ext = cur_el.get("extension_base", "")

            # Initialize an empty dictionary for the current complex type data
            cur_comp = {}

            # Check if the current element has an extension and if it starts
            # with "om:"
            if bool(cur_el_ext) and cur_el_ext.startswith("om:"):
                # Search for the corresponding complex type in complex_data
                for comp_key in complex_data:
                    if cur_el_ext.replace("om:", "") in comp_key:
                        cur_comp = complex_data[comp_key]
                        break

                # Merge the current element data with the corresponding complex
                # type data
                element_data_copy[key] = self.merge_dicts(cur_el, cur_comp)

                # Update the extension base, if any
                element_data_copy[key]["extension_base"] = cur_comp.get(
                    "extension_base", ""
                )

                # If the merged complexType has its own extension, recursively
                # merge again
                if bool(element_data_copy[key]["extension_base"]):
                    element_data_copy[key] = self.merge_extensions(
                        {key: element_data_copy[key]}, complex_data
                    )[key]

            # Check if the current element has an extension and if it starts
            # with "xs:"
            elif bool(cur_el_ext) and cur_el_ext.startswith("xs:"):
                # Update the type to the base type of the extension
                element_data_copy[key]["info"]["base_type"] = cur_el_ext

            # If no extension, continue to the next element
            else:
                continue

        # Return the merged element data
        return element_data_copy

    def merged_elements(self, element_data: dict, complex_data: dict) -> dict:
        """Merge complex type information into element data based on their types.

        This function iterates over element data and merges corresponding complex type
        information based on the type of each element. It updates the element data with
        the merged information.

        Parameters
        ----------
        element_data : dict
          The dictionary containing element data to be merged.

        complex_data : dict
          The dictionary containing complex type data to merge from.

        Returns
        -------
        dict:
          A dictionary with the merged element and complex type data.

        """
        # Create a copy of element_data to avoid modifying the original
        element_data_copy = element_data.copy()

        # Iterate over each item in the copied element data
        for key, value in element_data_copy.items():
            # Get the current element data
            cur_el = value

            # Get the type of the current element
            cur_el_type = cur_el["info"].get("type", {})

            # Initialize an empty dictionary for the current complex type data
            cur_comp = {}

            # Check if the current element has a type
            if bool(cur_el_type):
                # Search for the corresponding complex type in complex_data
                for comp_key in complex_data:
                    if cur_el_type.replace("om:", "") in comp_key:
                        cur_comp = complex_data[comp_key]
                        break

                # Merge the current element data with the corresponding complex
                # type data
                element_data_copy[key] = self.merge_dicts(cur_el, cur_comp)
            else:
                continue

        # Return the merged element data
        return element_data_copy

    def child_finder(
        self, child_name: str, child_type: str, data: dict
    ) -> tuple[int, str] | None:
        """Find a child element in the data based on its name and type.

        This function searches through the provided data dictionary to find a child
        element that matches the given name and type. It returns a tuple with the key of
        the found child.

        Parameters
        ----------
        child_name : str
          The name of the child element to find.

        child_type : str
          The type of the child element to find. If None, the search is based on name
          only.

        data : dict
          The dictionary containing the data to search through.

        Returns
        -------
        tuple[int, str] :
          The key of the found child element.

        """
        # Initialize the variable to store the found child key
        child = None

        # Check if the child type is None
        if child_type is None:
            # Iterate over the keys in the data dictionary
            for key in data:
                # If the child name is in the key, store the key and break the
                # loop
                if child_name in key:
                    child = key
                    break
        else:
            # Iterate over the keys in the data dictionary
            for key in data:
                # If the child name is in the key and the type matches, store
                # the key and break the loop
                if (
                    child_name in key
                    and data[key]["info"].get("type", "") == child_type
                ):
                    child = key
                    break

        # Return the found child key
        return child

    def finalize_metadata(self, element_data: dict) -> dict:
        """Finalize the metadata for each element in the given data dictionary.

        This function reorganizes and finalizes the metadata for each element in the
        provided dictionary. It moves attribute information into the ``info``
        subdictionary and updates children metadata with occurrence information.

        Parameters
        ----------
        element_data : dict
          The dictionary containing element data to finalize.

        Returns
        -------
        dict :
          The dictionary with finalized metadata for each element.

        """
        # Iterate over each key-value pair in the element_data dictionary
        for _, value in element_data.items():
            # Move the attributes subdictionary into the 'info' subdictionary
            value["info"]["attributes"] = {}

            # Iterate over each attribute in the attributes subdictionary
            for attr in list(value["attributes"].keys()):
                # Update the attributes in 'info' with the 'use' value of each
                # attribute
                value["info"]["attributes"].update(
                    {attr: value["attributes"][attr]["info"].get("use", "")}
                )

            # Initialize the 'children' subdictionary in 'info'
            value["info"]["children"] = {}

            # Check if there are any children in the element
            if kids := value.get("children", {}).items():
                # Iterate over each child key-value pair
                for kid_key, kid_value in kids:
                    # Find the child element in element_data and get its
                    # occurrence information
                    child_info = element_data[
                        self.child_finder(kid_key, kid_value, element_data)
                    ]["info"]
                    min_occurs = child_info.get("minOccurs", "1")
                    max_occurs = child_info.get("maxOccurs", "1")

                    # Update the children subdictionary in 'info' with the
                    # occurrence information
                    value["info"]["children"].update(
                        {kid_key: {"minOccurs": min_occurs, "maxOccurs": max_occurs}}
                    )

        # Return the updated element_data dictionary
        return element_data

    def generate_class_code(
        self,
        element_data: dict,
        complex_data: dict,
        indent: int = 0,
        node_type: str = "",
    ) -> list[str]:
        """Generate Python class code for XML elements and attributes.

        This function creates Python class definitions for XML elements and attributes
        using the provided element data and complex data. It handles different node
        types and generates class structures with appropriate attributes, properties,
        and metadata.

        Parameters
        ----------
        element_data : dict
          The dictionary containing data for XML elements.

        complex_data : dict
          The dictionary containing data for complex types.

        indent : int, default 0
          The indentation level for the generated class code.

        node_type : str, default ""
          The type of node being processed (e.g., "attributes").

        Returns
        -------
        list[str] :
          A list of strings representing the generated Python class code.

        """
        # Initialize an empty list to hold the lines of generated code
        lines = []

        # Create an indentation string based on the current indentation level
        indent_str = "    " * indent

        # Iterate over each key-value pair in the element data
        for key, value in element_data.items():
            # Determine the class name and node_name based on whether the node is an
            # attribute or not
            if node_type == "attributes":
                class_name = f"_{key[:1].upper() + key[1:]}"
                node_name = key
            else:
                class_name = f"OM{key[0]}" + key[1][:1].upper() + key[1][1:]
                node_name = key[1]

            # Extract the documentation text for the current element
            doc_text = element_data[key]["docs"]

            # Append the class definition to the lines list
            if node_type == "attributes":
                lines.append(f"{indent_str}class {class_name}(AttributeNode):")
            elif value["info"].get("type", "").startswith("xs:"):
                # Append an empty line to separate sections in the generated
                # code
                lines.append("")
                lines.append(f"{indent_str}class {class_name}(ElementNode):")
            else:
                # Append an empty line to separate sections in the generated
                # code
                lines.append("")
                lines.append(f"{indent_str}class {class_name}(Node):")

            # Add short docstring
            lines.append(f'{indent_str}    """Generated class for \'{node_name}\'."""')
            lines.append("")

            # Append the __init__ method definition to the lines list
            lines.append(
                f"{indent_str}    def __init__(self, xml_tree: etree._ElementTree, parent: Node | None = None) -> None:"
            )
            lines.append(
                f"{indent_str}        super().__init__(node_name='{node_name}', xml_tree=xml_tree, om_version={self.om_version})"
            )
            lines.append(f"{indent_str}        self._parent: Node | None = parent")
            lines.append(f'{indent_str}        self._doc: str = """{doc_text}"""')
            lines.append(f"{indent_str}        self._metadata: dict = {value['info']}")

            # Process the attributes of the current element
            attrs = element_data[key].get("attributes", {})
            if attrs and isinstance(attrs, dict):
                for attr_key, _ in attrs.items():
                    sub_class_name = "_" + attr_key[:1].upper() + attr_key[1:]
                    lines.append(
                        f"{indent_str}        self.{attr_key}: {class_name}.{sub_class_name} = self.{sub_class_name}(xml_tree=xml_tree, parent=self)"
                    )

            # Get the children of the current element
            kids = element_data[key].get("children", {})
            kids_property = {}

            # Check if kids is a dictionary and is not empty
            if kids and isinstance(kids, dict):
                # Iterate over each child key-value pair in kids
                for kid_key, kid_value in kids.items():
                    kid_name = kid_value
                    # If the kid_name is None, find the corresponding element in
                    # element_data
                    if kid_name is None:
                        for ele_key in element_data:
                            if kid_key in ele_key:
                                # Construct the class name for the child element
                                kid_name = (
                                    f"OM{ele_key[0]}"
                                    + ele_key[1][:1].upper()
                                    + ele_key[1][1:]
                                )
                                break
                    else:
                        # If kid_name is not None, match the type with
                        # element_data
                        for ele_key in element_data:
                            if (
                                kid_key in ele_key
                                and element_data[ele_key]["info"].get("type", "")
                                == kid_value
                            ):
                                # Construct the class name for the child element
                                kid_name = (
                                    f"OM{ele_key[0]}"
                                    + ele_key[1][:1].upper()
                                    + ele_key[1][1:]
                                )
                                break

                    # Append the child attribute to the lines list
                    lines.append(
                        f"{indent_str}        self._{kid_key}: {kid_name} | None = None"
                    )
                    kids_property[kid_key] = kid_name

                # Append an empty line to separate sections in the generated
                # code
                lines.append("")

            # If there are children, generate properties for them
            if kids:
                for prop_name, prop_key in kids_property.items():
                    lines.append(f"{indent_str}    @property")
                    lines.append(
                        f"{indent_str}    def {prop_name}(self) -> '{prop_key}':"
                    )
                    # Add short docstring
                    lines.append(
                        f'{indent_str}        """Initialize \'{prop_name}\'."""'
                    )
                    lines.append(f"{indent_str}        if self._{prop_name} is None:")
                    lines.append(
                        f"{indent_str}            self._{prop_name} = {prop_key}(xml_tree=self.xml_tree, parent=self)"
                    )
                    lines.append(f"{indent_str}        return self._{prop_name}")
                    lines.append("")
            else:
                # If there are no children, append an empty line for code
                # structure
                lines.append("")

            # If there are attributes, generate class code for them recursively
            if attrs and isinstance(attrs, dict):
                for key, value in attrs.items():
                    lines.extend(
                        self.generate_class_code(
                            {key: value},
                            complex_data,
                            indent=indent + 1,
                            node_type="attributes",
                        )
                    )

        # Return the generated lines of class code
        return lines
