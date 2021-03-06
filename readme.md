# Scala Viz Tools

Generation of a dashboard that lives alongside you as you're working in the repl and shows various visual elements.

A playground for all of our widgets, among other things.

Components
* Layout : Ability to generate areas, each area owned by a widget
* Widgets: Library of various widgets
* Repl   : the environment in which you work.

Implementation:
* Layout : golden layout
* Widgets: Plotly?
* Repl   : Ammonite

Alternates
* Layout: PhosphorJS, DockSpawn

Milestone 1:
Ability to define a layout, kick off the server process that watches it, send widgets to it.

## Ideas

* Partial updates, so only the widgets that have actually changed swap themselves out
  Rather than deep-equals, I could implement a last-updated timestamp
* Couple difference patterns:
    1. Story : Just keeps adding Divs
    2. Dashboard : Layout and you get one screen

## Milestone 1: Notes

* Scala to write out a configuration
* Example to load it from file
* components that understand those configs
* Probably re-add header with some of my own controls

## Milestone 2: Notes

* Websockets in scala spray? Have we solved that one? Check out how

## Usability Notes

To use ammonite shell as your repl for interacting with this
    sbt scala-viz-repl/test:console

## Alternate ideas

* Looking at Zeppelin. Can I get it running with a Tableness project?
