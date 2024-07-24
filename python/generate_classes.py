#!/usr/bin/env python3

import argparse
import os
import xsd_parser


def main(
    om_version: int | str, input_file: str | os.PathLike, output_file: str | os.PathLike
) -> None:
    """Parse OpenMalaria schema and generate Python classes.

    The classes will be generated from the information found in the XML schema
    definiton and their inheritance will represent the XML tree structure.

    Parameters
    ----------
    om_version : int or str
      The targeted OpenMalaria schema version.

    input_file : str or os.PathLike
      Path to the file to parse.

    input_file : str or os.PathLike
      Path to the output file.

    """
    parser = xsd_parser.XSDparser(om_version, input_file)
    parser.check_handled_tags()
    parser.generate_classes(output_file)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Parse OpenMalaria schema and generate Python classes"
    )
    parser.add_argument("om_version", type=int, help="OpenMalaria schema version")
    parser.add_argument("input_file", type=str, help="file to parse")
    parser.add_argument("output_file", type=str, help="output file")
    args = parser.parse_args()

    main(args.om_version, args.input_file, args.output_file)
