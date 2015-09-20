# forge-clj: Clojure in Minecraft

##Description

A very WIP implementation of clojure working with minecraft forge.

##Usage

To currently use this, first install forge as you normally would. Then, replace build.gradle with the version in this repository, and replace the src folder as well. You should now be able to run it like normal.

NOTE: DON'T USE THIS YET UNLESS YOU WANT TO HELP DEVELOP FORGE-CLJ. This really isn't at a point where you can use it to make other mods yet.

##Changelog

- Version 0.1.0: Lots of changes here: First, I added a working proxy system, to allow for client and common proxies. Second, I separated the test mod and forge-clj, so now they work independently. Finally, I fixed the ability to access certain fields (like creative-tab and sound-type), so that they work both in the dev environment and in a normal minecraft installation.

- Version 0.0.1: Initial Commit

## License

Copyright © 2015 Ryan Haney (Yoshiquest)

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
