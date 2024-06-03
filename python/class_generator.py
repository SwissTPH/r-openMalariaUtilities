#!/usr/bin/env python3

# Helper function to generate Python class code from the parsed data structure
from pprint import pprint


def merge_dicts(a: dict, b: dict) -> dict:
    """Recursively merge dictionary b into dictionary a.

    This function combines two dictionaries by recursively merging the values of
    common keys. Special handling is applied to keys named 'docs' and 'type' to
    concatenate their values.

    Parameters
    ----------
    a (dict): The target dictionary to merge into.

    b (dict): The source dictionary to merge from.

    Returns
    -------
    dict: A new dictionary with the merged content of a and b.
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
                target[key] = merge_dicts(target[key], value)
            # Otherwise, overwrite the target key with the source value
            else:
                target[key] = value
        # If the key does not exist in the target, add it from the source
        else:
            target[key] = value

    # Return the merged dictionary
    return target

def merge_extensions(element_data: dict, complex_data: dict) -> dict:
    """Merge extension data from complex types into element data.

    This function merges information from complex type extensions into the
    corresponding element data. It handles nested extensions and updates the
    base type of elements when needed.

    Parameters
    ----------

    element_data (dict): The dictionary containing element data to be merged.

    complex_data (dict): The dictionary containing complex type data to merge
      from.

    Returns
    -------
    dict: A dictionary with the merged element and complex type data.
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

        # Check if the current element has an extension and if it starts with "om:"
        if bool(cur_el_ext) and cur_el_ext.startswith("om:"):
            # Search for the corresponding complex type in complex_data
            for comp_key in complex_data.keys():
                if cur_el_ext.replace("om:", "") in comp_key:
                    cur_comp = complex_data[comp_key]
                    break

            # Merge the current element data with the corresponding complex type data
            element_data_copy[key] = merge_dicts(cur_el, cur_comp)

            # Update the extension base, if any
            element_data_copy[key]["extension_base"] = cur_comp.get("extension_base", "")

            # If the merged complexType has its own extension, recursively merge again
            if bool(element_data_copy[key]["extension_base"]):
                element_data_copy[key] = merge_extensions(
                    {key: element_data_copy[key]}, complex_data
                )[key]

        # Check if the current element has an extension and if it starts with "xs:"
        elif bool(cur_el_ext) and cur_el_ext.startswith("xs:"):
            # Update the type to the base type of the extension
            element_data_copy[key]["info"]["base_type"] = cur_el_ext

        # If no extension, continue to the next element
        else:
            continue

    # Return the merged element data
    return element_data_copy

def merged_elements(element_data: dict, complex_data: dict) -> dict:
    """Merge complex type information into element data based on their types.

    This function iterates over element data and merges corresponding complex
    type information based on the type of each element. It updates the element
    data with the merged information.

    Parameters
    ----------
    element_data (dict): The dictionary containing element data to be merged.

    complex_data (dict): The dictionary containing complex type data to merge
      from.

    Returns
    -------
    dict: A dictionary with the merged element and complex type data.
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
            for comp_key in complex_data.keys():
                if cur_el_type.replace("om:", "") in comp_key:
                    cur_comp = complex_data[comp_key]
                    break

            # Merge the current element data with the corresponding complex type data
            element_data_copy[key] = merge_dicts(cur_el, cur_comp)
        else:
            continue

    # Return the merged element data
    return element_data_copy

# pprint(element_data[(11, "option")])
merged_element_data = merged_elements(element_data, complex_data)

# pprint(merged_element_data[(11, "option")])

# pprint(merged_element_data[(5, "continuous")])
# for key in merged_element_data.keys():
#     if "secondReleaseDays" in key:
#         print(key)
#         pprint(merged_element_data[key])

merged_element_data = merge_extensions(merged_element_data, complex_data)
# pprint(merged_element_data[(11, "option")])

# pprint(merged_element_data[(5, "continuous")])
# for key in merged_element_data.keys():
#     if "secondReleaseDays" in key:
#         print(key)
#         pprint(merged_element_data[key])

def child_finder(child_name: str, child_type: str, data: dict) -> tuple[int, str]:
    """Finds a child element in the data based on its name and type.

    This function searches through the provided data dictionary to find a child
    element that matches the given name and type. It returns a tuple with the
    key of the found child.

    Parameters
    ----------
    child_name (str): The name of the child element to find.

    child_type (str): The type of the child element to find. If None, the search
      is based on name only.

    data (dict): The dictionary containing the data to search through.

    Returns
    -------
    tuple[int, str]: The key of the found child element.

    """
    # Initialize the variable to store the found child key
    child = None

    # Check if the child type is None
    if child_type is None:
        # Iterate over the keys in the data dictionary
        for key in data.keys():
            # If the child name is in the key, store the key and break the loop
            if child_name in key:
                child = key
                break
    else:
        # Iterate over the keys in the data dictionary
        for key in data.keys():
            # If the child name is in the key and the type matches, store the
            # key and break the loop
            if child_name in key and data[key]["info"].get("type", "") == child_type:
                child = key
                break

    # Return the found child key
    return child

def finalize_metadata(element_data: dict) -> dict:
    """Finalize the metadata for each element in the given data dictionary.

    This function reorganizes and finalizes the metadata for each element in the
    provided dictionary. It moves attribute information into the 'info'
    subdictionary and updates children metadata with occurrence information.

    Parameters
    ----------
    element_data (dict): The dictionary containing element data to finalize.

    Returns
    -------
    dict: The dictionary with finalized metadata for each element.
    """
    # Iterate over each key-value pair in the element_data dictionary
    for key, value in element_data.items():
        # Move the attributes subdictionary into the 'info' subdictionary
        value["info"]["attributes"] = {}

        # Iterate over each attribute in the attributes subdictionary
        for attr in list(value["attributes"].keys()):
            # Update the attributes in 'info' with the 'use' value of each
            # attribute
            value["info"]["attributes"].update({attr: value["attributes"][attr]["info"].get("use", "")})

        # Initialize the 'children' subdictionary in 'info'
        value["info"]["children"] = {}

        # Check if there are any children in the element
        if (kids := value.get("children", {}).items()):
            # Iterate over each child key-value pair
            for kid_key, kid_value in kids:
                # Find the child element in element_data and get its occurrence
                # information
                child_info = element_data[child_finder(kid_key, kid_value, element_data)]["info"]
                min_occurs = child_info.get("minOccurs", "1")
                max_occurs = child_info.get("maxOccurs", "1")

                # Update the children subdictionary in 'info' with the
                # occurrence information
                value["info"]["children"].update({kid_key: {"minOccurs": min_occurs, "maxOccurs": max_occurs}})

    # Return the updated element_data dictionary
    return element_data

merged_element_data = finalize_metadata(merged_element_data)

def generate_class_code(element_data: dict, complex_data: dict, indent: int = 0, node_type: str = "") -> list[str]:
    """Generates Python class code for XML elements and attributes based on the
    provided data.

    This function creates Python class definitions for XML elements and
    attributes using the provided element data and complex data. It handles
    different node types and generates class structures with appropriate
    attributes, properties, and metadata.

    Parameters
    ----------
    element_data (dict): The dictionary containing data for XML elements.

    complex_data (dict): The dictionary containing data for complex types.

    indent (int): The indentation level for the generated class code. Default is
      0. This parameter is optional.

    node_type (str): The type of node being processed (e.g., "attributes").
      Default is an empty string. This parameter is optional.

    Returns
    -------
    list[str]: A list of strings representing the generated Python class code.
    """
     # Initialize an empty list to hold the lines of generated code
    lines = []

    # Create an indentation string based on the current indentation level
    indent_str = "    " * indent

    # Iterate over each key-value pair in the element data
    for key, value in element_data.items():
        # Determine the class name and cname based on whether the node is an
        # attribute or not
        if node_type == "attributes":
            class_name = f"_{key[:1].upper() + key[1:]}"
            cname = key
        else:
            class_name = f"OM{key[0]}" + key[1][:1].upper() + key[1][1:]
            cname = key[1]

        # Extract the documentation text for the current element
        doc_text = element_data[key]["docs"]

        # Append the class definition to the lines list
        if node_type == "attributes":
            lines.append(f"{indent_str}class {class_name}(AttributeBase):")
        elif value["info"].get("type", "").startswith("xs:"):
            lines.append(f"{indent_str}class {class_name}(ElementBase):")
        else:
            lines.append(f"{indent_str}class {class_name}(BaseClass):")

        # Append the __init__ method definition to the lines list
        lines.append(f"{indent_str}    def __init__(self, xml_tree: etree.Element, parent: BaseClass | None = None) -> None:")
        lines.append(
            f"{indent_str}        super().__init__(cname='{cname}', xml_tree=xml_tree)"
        )
        lines.append(f"{indent_str}        self.parent = parent")
        lines.append(f'{indent_str}        self.doc: str = """{doc_text}"""')
        lines.append(f"{indent_str}        self.metadata: dict = {value['info']}")

        # Process the attributes of the current element
        attrs = element_data[key].get("attributes", {})
        if attrs and isinstance(attrs, dict):
            for attr_key, attr_value in attrs.items():
                sub_class_name = "_" + attr_key[:1].upper() + attr_key[1:]
                lines.append(
                    f"{indent_str}        self.{attr_key}: '{sub_class_name}' = self.{sub_class_name}(xml_tree=xml_tree, parent=self)"
                )

        # Get the children of the current element
        kids = element_data[key].get("children", {})
        kids_property = {}

        # Check if kids is a dictionary and is not empty
        if kids and isinstance(kids, dict):
            # Iterate over each child key-value pair in kids
            for kid_key, kid_value in kids.items():
                kid_name = kid_value
                # If the kid_name is None, find the corresponding element in element_data
                if kid_name is None:
                    for ele_key in element_data.keys():
                        if kid_key in ele_key:
                            # Construct the class name for the child element
                            kid_name = (
                                f"OM{ele_key[0]}"
                                + ele_key[1][:1].upper()
                                + ele_key[1][1:]
                            )
                            break
                else:
                    # If kid_name is not None, match the type with element_data
                    for ele_key in element_data.keys():
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
                lines.append(f"{indent_str}        self._{kid_key}: '{kid_name} | None' = None")
                kids_property[kid_key] = kid_name

        # Append an empty line to separate sections in the generated code
        lines.append("")

        # If there are children, generate properties for them
        if kids:
            for prop_name, prop_key in kids_property.items():
                lines.append(f"{indent_str}    @property")
                lines.append(f"{indent_str}    def {prop_name}(self) -> '{prop_key}':")
                lines.append(f"{indent_str}        if self._{prop_name} is None:")
                lines.append(f"{indent_str}            self._{prop_name} = {prop_key}(xml_tree=self._xml_tree, parent=self)")
                lines.append(f"{indent_str}        return self._{prop_name}")
                lines.append("")
        else:
            # If there are no children, append an empty line for code structure
            lines.append("")

        # If there are attributes, generate class code for them recursively
        if attrs and isinstance(attrs, dict):
            for key, value in attrs.items():
                lines.extend(
                    generate_class_code(
                        {key: value},
                        complex_data,
                        indent=indent + 1,
                        node_type="attributes",
                    )
                )

    # Return the generated lines of class code
    return lines


# Generate Python class code
header = []
header.append("from lxml import etree")
header.append("from base_class import BaseClass, AttributeBase, ElementBase")
header.append("")
header_code = "\n".join(header)

class_code_lines = generate_class_code(merged_element_data, complex_data)
class_code = "\n".join(class_code_lines)
f = open("classcode.py", "w")
f.write(header_code)
f.write("\n")
f.write(class_code)
f.close()
