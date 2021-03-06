# Simple viewer of ArcGrid geospatial data written in Haskell

## Synopsis
A simple viewer for ESRI/ArcInfo (ArcGrid) files. A user can pan, zoom and
rotate the rendered dataset with keyboard and mouse (see _Controls_).


![alt text](https://github.com/nbrk/arcgrid-viewer/blob/master/doc/scr_alps3.png "Screenshot")


The program uses shades of red (by default) normalized by minimum and maximum
value in the VAT of the curent dataset. Black colour represents lower value than
the red one.

## Installation
The program depends on `gloss` and `arcgrid` libraries. The easiest way is to
install utulity `stack` and build the software from the source code.

Currently, as this:

``` sh
$ git clone https://github.com/nbrk/arcgrid.git
$ git clone https://github.com/nbrk/arcgrid-viewer.git
$ cd arcgrid-viewer
$ stack install
# Usually stack will install the program binary in ~/.local/bin .
# You can add it to PATH of you want.
$ ~/.local/bin/arcgrid-viewer ../arcgrid/sample/alps_huge.asc
```

## Usage
The program supports rendering the dataset in few color schemes (red, black and
white, a spectre) in both vector and raster modes. 

```
Usage: arcgrid-viewer [OPTION...] <file>
  -c MODE  --color=MODE  color scheme: red|bw|fancy
  -r MODE  --mode=MODE   rendering mode: vector|raster
```

The defaults are to render **in raster** with **red gradient**.
Please keep in mind that big datasets take time and resources to parse and render!

There is some sample `.asc` elevation data from the Alps. It resides in `arcgrid`
library and is [located](https://github.com/nbrk/arcgrid/tree/master/sample) in `arcgrid/sample`.

## Controls
The program uses `gloss` library for rendering and display. Therefore, the
controls are:

```
* Quit
  - esc-key

* Move Viewport
  - arrow keys
  - left-click drag

* Zoom Viewport
  - page up/down-keys
  - control-left-click drag
  - right-click drag
  - mouse wheel

* Rotate Viewport
  - home/end-keys
  - alt-left-click drag

* Reset Viewport
  r-key
```

## TODO
- Optimization. The viewer is too damn slow!
- Maybe restrict display only to a selected rectangle.
