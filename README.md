# Simple viewer of ArcGrid geospatial data written in Haskell

## Synopsis
A simple viewer for ESRI/ArcInfo (ArcGrid) files. A user can pan, zoom and
rotate the rendered dataset with keyboard and mouse (see _Controls_).

The program uses shades of red normalized by minimum and maximum value in the
VAT of the curent dataset. Black colour represents lower value than the red.

## Installation
Currently, as this:

``` sh
$ git clone https://github.com/nbrk/arcgrid.git
$ git clone https://github.com/nbrk/arcgrid-viewer.git
$ cd arcgrid-viewer
$ stack build
$ # read a set (currently, only .asc files)
$ stack exec arcgrid-viewer ../arcgrid/app/alps_big.asc
```

## Usage

``` sh
arcgrid-viewer <file.arc>
```

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
