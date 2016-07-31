# Randomized Evolution List

Between July 12th and July 26th,
[TwitchPlaysPokemon](http://www.twitch.tv/twitchplayspokemon) played a game of
Pokémon Alpha Sapphire randomized using [pk3DS](https://github.com/kwsch/pk3DS).
The randomization included evolutions such that a Torchic would not necessarily
evolve into a Combusken.

The rules were known to be: A Pokemon's new evolution would match at least one
of the types of its natural evolution, and that the the new evolution would have
a BST within 20% of the natural evolution. The pages produced by this build will
show what these rules allow a Pokémon species to evolve into or from.

There are three known points of inaccuracy. First, the randomizer will not allow
a Pokémon to evolve into itself, but this code was never changed to account for
that. Second, the Randomizer, if it tries and fails to find an evolution within
the allowed parameters, will increase the upper range of allowed BSTs. So, even
though Flygon's BST is 30% more than Dugtrio's BST, enough luck allows a Diglet
to evolve into a Flygon. Lastly, BSTs were randomized as
well, although this was not figured out until someone noticed that Mega
Rayquaza's Special Attack was significantly lower than Rayquaza's Special
Attack, and that Mega Rayquaza's HP was higher than Rayquaza's HP. If
no one noticed before then, the difference couldn't have been that much, but it
would explain how Chansey was allowed to evolve into Wigglytuff.


## Build

1. Get [sbt](http://www.scala-sbt.org/)
2. Clone repo
3. In the repo's root directory, run the command `sbt web-stage`
4. Results are html files in the `target/web/stage` directory

