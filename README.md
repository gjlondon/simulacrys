= README

This project is an attempt to create a realistic,
high fidelity simulation of a functional economic (and, in time, perhaps also political) system.

The original impetus for this project was to assist in building a believable simulation.world for an original Dungeons and Dragons
campaign setting. As I started imagining a fictional simulation.world for my friends' characters to inhabit, I got increasingly
obsessed and concerned with how to construct a simulation.world that felt natural and alive. As a very experienced video gamer
and consumer of various fantastical movies, books, and assorted media I'm well aware that the is a constant tension
between realism in setting and convenience for both storytelling (inasmuch as characters cannot keep track of nearly
as many people and places as exist in the real simulation.world), and construction (inasmuch as simulation.world with too many people is
expensive to design and depict).

Nevertheless, as a professional programmer, erstwhile macroeconomic analyst, and an inveterate obsessive I've found myself
drawn away from crafting a story and towards the challenge of crafting a simulation.world. And so herein you'll find my best attempt
to develop a computer program populated with people who behave a realistically as possible to achieve goals by means of
definite simulation.actions in a a concrete environment.

Because the goal is to create emergent macro-economic behavior that is nevertheless visible and interpretable at a
"human scale" (I do eventually want to bring player characters into this simulation.world), it makes rely on agent-based modeling
techniques rather than classical macro-economic modeling. The fact that I know hardly anything about agent-based modeling
or about classical macro-economic modeling will hopefully not be too great a barrier.

== The Domain

I will attempt to model a single "closed" economic system consisting effectively of a single fictional country. In the future,
I may try to model a system with multiple countries (and thus exchange rates) but in my first iteration I'll stick to one.

==== Locations

I'll refer to this country as *"The Kingdom"*. The kingdom will consist of a continuous geographic expanse which I will model
as a square grid of 100,000x100,000 *hectares*. Each hectare will have exactly one type of primary economic usage,
e.g. agricultural, light industrial, educational, fortified, etc. A *simulation.location* is a conjoined set of hectares with some
common unified significance, e.g. *a village*, *a town*, *a city*, *a farm*, etc.

==== Inhabitants

The kingdom will be occupied by *People*. People, like in the real simulation.world, will have definite simulation.demographic characteristics
including age, gender, height, and weight. They will have *needs* such as eating, drinking, and shelter. And they will
have desires such as wealth and...well, for simplicity primarily wealth.

People will be organized into *Families*. The simulation will start with a pre-existing simulation.populace and then the simulation.populace
will expand as *Children* are born and contract as people *Die*.

People who are not able to satisfy their basic needs will die. So each simulation.person will be programmed with a core set of
*Policies* that guide them to take *Actions* which promote survival.

==== Time

As in life, time in the simulation flies relentlessly forward. Time in the simulation is governed by a single global
simulation.clock that moves forward in one-hour ticks. At the beginning of each tick, each simulation.person will make a decision based on
their policies about which action to take ove the coming hour (which choice may be simply to continue a previously undertaken
action which requires more than an hour to complete). At the beginning of the next tick, each simulation.person will observe the
results of their action on the previous tick, observe the current state of their locally observable environment, and
use that information to choose an action for the next round.

The simulation will progress indefinitely until everyone in the Kingdom dies.