#Changelog

##Minecraft 1.8.9

###Version 0.6.0

- Updated EVERYTHING to 1.8.9, including blocks, items, rendering, the whole package. The only thing missing is all of the chunk provider stuff, which I removed for now until I get around to taking a second look at it.

- REPL support was removed for now. Shading a second jar into forge-clj really wasn't worth it, so for now, if you need a repl, simply compile the tools.nrepl in your project, and start a server from there.

- I had to add a workaround for a new check in Minecraft. Forge-clj now has to register the blocks in the Blocks class in net.minecraft.init at compile time, as well as use reflection to trick Bootstrap that it has already been registered. This may cause unforseen problems, but from what I've observed everything seems to be working fine for now.

- Added a brand new system for Blockstates (as a result the old metadata system no longer exists).

- Made it so that blocks and items will now set their unlocalized names to the name given in defblock/defitem/etc. automatically if not provided.

- Wrapped around some things in defobj so that there is always an implicit "this" active, that refers to the current instance.

- Most of the registration functions have all been merged into "register". It SHOULD work, but if it doesn't register properly for some reason you should call the underlying registration function instead.

- Warning: networking is still somewhat incomplete, as there is no thread safety checking currently, and you will have to push things into the proper thread yourself. I plan on revamping this system at a later date.

- Moved the changelog to a separate file and reformatted it completely.

##Minecraft 1.7.10

###Version 0.5.2

- Added the ability to make biomes.

- Extended upon defobj so that you can now optionally make it generate a class underneath for the purpose of accessing protected fields.

- Also added some support for custom chunk providers, though unfortunately I was unable to replicate Minecraft's terrain generation function, due to some slowness in the language as well as general complexity issues. If you wish to make a chunk provider for a forge-clj mod, I would recommend just copying the chunk provider from Java, and then reference it via inter-op. My apologies for not being able to figure this out, and even greater apologies for spending a month TRYING to figure it out and failing.

###Version 0.5.1

- Added docstrings to the code.

- Added 2 new useful macros: defclass and with-prefix.

- Also rearranged the namespaces a bit. The deftab macro is now in forge-clj.items, and many of the more utility-based functions in the core (gen-classname, get-fullname, etc.) are now in forge-clj.util. I also added a new namespace called forge-clj.registry, that contains all of the register functions and such (except for the network stuff). Also made forge-clj.client.registry, which is the same, but is for the client side.

###Version 0.5.0

- Added REPL support. To use it, first add the :repl keyword to the defmod macro, and set it to your port number or set it to true to use the default port of 7888. Then just connect to it with another tool (if using leiningen do "lein repl :connect \<portnumber\>").

- Also finished adding support for GUIs (with inventory).

###Version 0.4.1

- Made it so that forge-clj works on dedicated servers now. Make sure that your client namespace is not required in your core, and you use the full namespace name when specifying it now. It'll auto-require the client ns if this is on the integrated client.

- Also did some things to the NBT system, fixing special strings such as "nil", and adding support for maps, vectors, lists, and itemstacks.

###Version 0.4.0

- Added a very large system for rendering models. Currently only works for tile entities, and techne isn't supported (yet).

- Also added a few more minor things, such as the ability to create your own creative tab.

- Finally, I entirely revamped the organization of the mod, by splitting it into parts. Both forge-clj and test-mod are far easier to read now that everything isn't just in a single file.

- As a sidenote, as of this version I've started to use a custom program to help me auto-distribute each new version. Hopefully this means that new versions will be coming out faster than normal, since it won't be as much work for me. ((NOTE: THIS WAS REMOVED WHEN I MOVED TO GIT RELEASES))

###Version 0.3.0

- First off, the Tile Entity system was revamped, and the data is now stored inside an atom within the class instance rather than a global atom. This new class also supports hash-map-like syntax for getting and setting values.

- Next, I added the ability to create custom packets. The system takes data as a map, converts it to nbt, and sends it using the network system implemented.

- I then added the ability to create an event handler. You specify the name as a keyword, and the event Class itself in the :event tag.

- Finally, I added the ability to add custom entity properties, using a similar system to that of the Tile Entities.

###Version 0.2.2

- This update focuses on Tile Entities and NBT data. First, you now have the ability to create Tile Entities. Note that for Tile Entities, a CLASS is generated, rather than an instance like normal, so it'll be a bit different to work with.

- Next, I added the ability to convert Minecraft's NBT data into a Clojure map and back.

- Finally, I added a system that will automatically store this converted nbt data in an atom of your choice, using a unique key underneath in order to separate the instances.

###Version 0.2.1

- Improved the defx (defblock, defitem, etc.) macros. Instead of being the same thing over and over again, I now have a defobj macro that they all use instead.

- I also removed the field functions (creative-tab, step-sound, etc.), since they were pretty redundent, were a pain to make and maintain, and the amount of code needed by the user really didn't change. Just use java interop instead.

- I also merged the register-generator function with the rest of the register functions, so now you only need to call 1 function again.

###Version 0.2.0

- I've been using BedrockMiner's tutorials to know what I need to implement, and I'm proud to say that all of the basic tutorials (except for potions) can be done in forge-clj (and most have been done within the testmod as well). It works pretty well now, though some refactoring needs to be done, especially with the defx macros.

- I also separated the dev and user version, because apparantly you can do that. Things such as metablocks/items, tools, food, armor, and basic world generation were added. Also added a download link now.

###Version 0.1.0

- First, I added a working proxy system, to allow for client and common proxies.

- Second, I separated the test mod and forge-clj, so now they work independently.

- Finally, I fixed the ability to access certain fields (like creative-tab and sound-type), so that they work both in the dev environment and in a normal minecraft installation.

###Version 0.0.1

- Initial Commit
