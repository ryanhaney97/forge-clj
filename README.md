#forge-clj: Clojure in Minecraft

##Description

A WIP implementation of Clojure working with Minecraft Forge. This is now somewhat useable!

Explaining everything here would be a hassle, so go look at the [test-mod](https://github.com/yoshiquest/test-mod "test-mod") I made. It assumes you already know Clojure for some things. I plan on making a separate tutorial on Clojure itself and using it with forge-clj later though.

WARNING: THIS HASN'T REALLY BEEN TESTED VERY MUCH ON OTHER MACHINES. THINGS MIGHT NOT WORK!

##Usage (for users)

Simply download the latest version of forge-clj, as well as the mod you want to run, and put them both in the mods folder.

This may or may not work, tell me if it doesn't.

##Usage (for modders)

First off, set up Forge as normal. Then download the latest version of the forge-clj dev kit, and extract its contents into the dev environment. Afterwords, be sure to rename the things in the build.gradle to use your package names and not mine.

If this doesn't work be sure to tell me so I can fix it.

##Downloads

- Version 0.2.0: [forge-clj](http://bit.ly/1FTJ5HO "forge-clj Version 0.2.0") [forge-clj devkit](http://bit.ly/1jOENra "forge-clj devkit Version 0.2.0")

##Changelog

- Version 0.2.0: Even more changes now. I've been using BedrockMiner's tutorials to know what I need to implement, and I'm proud to say that all of the basic tutorials (except for potions) can be done in forge-clj (and most have been done within the testmod as well). It works pretty well now, though some refactoring needs to be done, especially with the defx macros. I also separated the dev and user version, because apparantly you can do that. Things such as metablocks/items, tools, food, armor, and basic world generation were added. Also added a download link now.

- Version 0.1.0: Lots of changes here: First, I added a working proxy system, to allow for client and common proxies. Second, I separated the test mod and forge-clj, so now they work independently. Finally, I fixed the ability to access certain fields (like creative-tab and sound-type), so that they work both in the dev environment and in a normal minecraft installation.

- Version 0.0.1: Initial Commit

##License

Copyright © 2015 Ryan Haney (Yoshiquest)

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
