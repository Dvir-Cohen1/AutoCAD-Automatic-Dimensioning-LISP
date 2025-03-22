# AutoCAD Automatic Dimensioning LISP

This repository contains a custom AutoCAD LISP function designed to automate the dimensioning of polylines, lines, and arcs. The script offers two modes for dimensioning: an automatic method that processes objects and a manual method where users can select two points interactively. Additionally, users can customize dimension placement via a dialog box.

## Features

- **Automatic Dimensioning**: Automatically place dimensions on selected polylines, lines, and arcs.
- **Manual Dimensioning**: Manually select two points to place dimensions.
- **Customizable Options**: Configure dimension position for lines (above or below) and arcs (inside or outside) using a dialog box.

## Requirements

- AutoCAD (version supporting LISP scripting)
- Basic familiarity with AutoCAD's dimensioning tools

## Installation

1. Download or clone this repository to your local machine.
2. Load the LISP script into AutoCAD using the `APPLOAD` command.
3. Run the `AD` command to start the dimensioning tool.

## Usage

1. **Automatic Mode**: Run the `AD` command and choose **Original** (default) for automatic dimensioning. Select objects (polylines, lines, arcs) to apply dimensions.
2. **Manual Mode**: Run the `AD` command and choose **PickPoints** to manually select two points and place a dimension.

## Customization

The script allows users to customize the position of the dimensions via the provided dialog. You can set the dimension position for lines to be above or below and for arcs to be inside or outside.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
