# Hiten

WARNING: This is very early stage, experimental software. Don't expect
it to be useful.

Hiten allows you to control a Google Chrome session from an Emacs
buffer. Why would you want to do that? Well, for one thing, it allows
you to browse any website that Chrome can load within Emacs, including
fancy webapps using Javascript that are not likely to work using
text-based browsers such as eww, w3m or elinks.

## Prerequisites
* node.js
* pandoc
* Google Chrome and chromedriver

## Installation
* Run `yarn install` (or `npm install` if you don't have Yarn)

## Usage
* In a terminal, run `chromedriver --port=4444 --url-base=/wd/hub`
* In Emacs, load `hiten.el` from this repository, then run `M-x
  hiten`. (Your `node` executable needs to be available on Emacs'
  `exec-path`.)
