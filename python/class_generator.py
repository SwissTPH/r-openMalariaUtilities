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
    element_data_copy = element_data.copy()
    for key, value in element_data_copy.items():
        # Loop over dict and merge with complex_data
        cur_el = value
        cur_el_ext = cur_el.get("extension_base", "")
        cur_comp = {}
        if bool(cur_el_ext) and cur_el_ext.startswith("om:"):
            for comp_key in complex_data.keys():
                if cur_el_ext.replace("om:", "") in comp_key:
                    cur_comp = complex_data[comp_key]
                    break
            # merge
            element_data_copy[key] = merge_dicts(cur_el, cur_comp)
            # update extension, if any
            element_data_copy[key]["extension_base"] = cur_comp.get(
                "extension_base", ""
            )
            # If the merged complexType has its own extension, recursively merge again
            if bool(element_data_copy[key]["extension_base"]):
                element_data_copy[key] = merge_extensions(
                    {key: element_data_copy[key]}, complex_data
                )[key]
        elif bool(cur_el_ext) and cur_el_ext.startswith("xs:"):
            # Update the type to the base type of the extension
            element_data_copy[key]["info"]["base_type"] = cur_el_ext
        else:
            continue
    return element_data_copy


def merged_elements(element_data: dict, complex_data: dict) -> dict:
    element_data_copy = element_data.copy()
    for key, value in element_data_copy.items():
        # Loop over dict and merge with complex_data
        cur_el = value
        cur_el_type = cur_el["info"].get("type", {})
        cur_comp = {}
        if bool(cur_el_type):
            for comp_key in complex_data.keys():
                if cur_el_type.replace("om:", "") in comp_key:
                    cur_comp = complex_data[comp_key]
                    break
            # merge
            # print("merging", comp_key, "into", key)
            element_data_copy[key] = merge_dicts(cur_el, cur_comp)
        else:
            continue
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

def child_finder(child_name: str, child_type: str, data) -> tuple[int, str]:
    if child_type is None:
        for key in data.keys():
            if child_name in key:
                child = key
                break
    else:
        for key in data.keys():
            if child_name in key and data[key]["info"].get("type", "") == child_type:
                child = key
                break
    return child

def finalize_metadata(element_data: dict) -> dict:
    for key, value in element_data.items():
        # Move subdicts a bit around
        value["info"]["attributes"] = {}
        for attr in list(value["attributes"].keys()):
            value["info"]["attributes"].update({attr: value["attributes"][attr]["info"].get("use", "")})

        # for kid in ["info"]["children_metadata"]["order"]
        value["info"]["children"] = {}
        if (kids := value.get("children", {}).items()):
            for kid_key, kid_value in kids:
                value["info"]["children"].update({kid_key: {"minOccurs": element_data[child_finder(kid_key, kid_value, element_data)]["info"].get("minOccurs", "1"),
                                                            "maxOccurs": element_data[child_finder(kid_key, kid_value, element_data)]["info"].get("maxOccurs", "1")}})

    return element_data

merged_element_data = finalize_metadata(merged_element_data)

def generate_class_code(element_data: dict, complex_data: dict, indent: int = 0, node_type: str = "") -> list[str]:
    lines = []
    indent_str = "    " * indent

    for key, value in element_data.items():
        if node_type == "attributes":
            class_name = f"_{key[:1].upper() + key[1:]}"
            cname = key
        else:
            class_name = f"OM{key[0]}" + key[1][:1].upper() + key[1][1:]
            cname = key[1]

        doc_text = element_data[key]["docs"]

        if node_type == "attributes":
            lines.append(f"{indent_str}class {class_name}(AttributeBase):")
        elif value["info"].get("type", "").startswith("xs:"):
            lines.append(f"{indent_str}class {class_name}(ElementBase):")
        else:
            lines.append(f"{indent_str}class {class_name}(BaseClass):")

        # Initialize method
        lines.append(f"{indent_str}    def __init__(self, xml_tree: etree.Element, parent: BaseClass | None = None) -> None:")
        lines.append(
            f"{indent_str}        super().__init__(cname='{cname}', xml_tree=xml_tree)"
        )
        lines.append(f"{indent_str}        self.parent = parent")
        lines.append(f'{indent_str}        self.doc: str = """{doc_text}"""')
        lines.append(f"{indent_str}        self.metadata: dict = {value['info']}")

        attrs = element_data[key].get("attributes", {})
        if attrs and isinstance(attrs, dict):
            for attr_key, attr_value in attrs.items():
                sub_class_name = "_" + attr_key[:1].upper() + attr_key[1:]
                lines.append(
                    f"{indent_str}        self.{attr_key}: '{sub_class_name}' = self.{sub_class_name}(xml_tree=xml_tree, parent=self)"
                )

        kids = element_data[key].get("children", {})
        kids_property = {}
        if kids and isinstance(kids, dict):
            for kid_key, kid_value in kids.items():
                kid_name = kid_value
                if kid_name is None:
                    for ele_key in element_data.keys():
                        if kid_key in ele_key:
                            kid_name = (
                                f"OM{ele_key[0]}"
                                + ele_key[1][:1].upper()
                                + ele_key[1][1:]
                            )
                            break
                else:
                    for ele_key in element_data.keys():
                        if (
                            kid_key in ele_key
                            and element_data[ele_key]["info"].get("type", "")
                            == kid_value
                        ):
                            kid_name = (
                                f"OM{ele_key[0]}"
                                + ele_key[1][:1].upper()
                                + ele_key[1][1:]
                            )
                            break

                lines.append(f"{indent_str}        self._{kid_key}: '{kid_name} | None' = None")
                kids_property[kid_key] = kid_name

        lines.append("")

        if kids:
            for prop_name, prop_key in kids_property.items():
                lines.append(f"{indent_str}    @property")
                lines.append(f"{indent_str}    def {prop_name}(self) -> '{prop_key}':")
                lines.append(f"{indent_str}        if self._{prop_name} is None:")
                lines.append(f"{indent_str}            self._{prop_name} = {prop_key}(xml_tree=self._xml_tree, parent=self)")
                lines.append(f"{indent_str}        return self._{prop_name}")
                lines.append("")
        else:
            lines.append("")

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
