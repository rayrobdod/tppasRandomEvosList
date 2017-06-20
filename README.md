# Randomized Evolution List

Occasionally, [TwitchPlaysPokemon](http://www.twitch.tv/twitchplayspokemon)
hosts runs of games that have been randomized. Starting with Twitch Plays
Randomized Alpha Sapphire in July of 2015, these randomized runs have
included randomized evolutions: Changing the game's evolution tables such that a
Torchic may not necessarily evolve into a Combusken but may evolve into, say, a
Lampent or a Boldore instead.

Before each run, the randomizer's settings are described to the public, for the
most part. Divulging enough to give a preview and/or feel of the run without
divulging enough to allow people to figure out the randomizer seed from the
first route's encounters. The 3DS games have been randomized using
[pk3DS](https://github.com/kwsch/pk3DS) and the DS games have been randomized
using the [Universal Pokémon Randomizer](https://github.com/Dabomstew/universal-pokemon-randomizer).
The new evolutions will have a similar BST to the vanilla evolution - however
the randomizer defines that. The new evolution may or may not be forced to share
a type with the vanilla evolution. The randomizer may or may not require Pokémon
to evolve into something with the same experience group. In one instance,
Pokémon types were randomized.

After each run, the randomizer logs are released.

The pages produced by this build will show the allowed evolutions and
prevolutions using the run's randomizer settings as well as an analysis of the
game's randomizer logs if said logs have been released.

If a randomizer fails to find a suitable evolution candidate upon it's first
pass, it will widen its search by expanding the acceptable BST range of the new
evolution result or temporarily ignoring other requirements. Even so, these
pages will only display the results assuming a successful first pass.


## Build

1. Get [sbt](http://www.scala-sbt.org/)
2. Clone repo
3. In the repo's root directory, run the command `sbt webStage`
4. Results are html files in the `target/web/stage` directory

* Most of the data is in [/src/main/data/](/src/main/data/).
  * [listOfPokemon.csv](/src/main/data/listOfPokemon.csv) is data about each Pokémon species - name, dex number, BST, type, etc.
  * The `**Evolutions.csv` files describe the evolutions observed in each game. 
* The page templates are in [/project/PageTemplates.scala](/project/PageTemplates.scala)
  and mostly use [ScalaTags](http://www.lihaoyi.com/scalatags/).
* The model classes are in [/project/package.scala](/project/package.scala).
* The computations are in [/project/ListOfPokemon.scala](/project/ListOfPokemon.scala).
* Static css and js files are located in [/src/main/public/](/src/main/public/).
