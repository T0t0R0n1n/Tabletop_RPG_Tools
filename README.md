# Tabletop RPG Tools
A compilation of easy-to-use custom functions and tools I've created in R to simulate character creation, economics, immigration, combat, or just to save myself time when playing tabletop RPGs like Dungeons &amp; Dragons.

These are still very much works in progress, but please feel free to fork, modify, and use these as you please. I hope to assemble them into a convenient package in the future.

# **PersonSim**

**Summary:** A basic function which requires users to input the number of individuals they would like to stimulate (```nPeople```). R will then generate a dataframe of rows equivalent to nPeople in length, with each row containing a unique combination of a:  
1) First Name,  
2) Surname,  
3) Gender,  
4) Culture, and  
5) Familial Background.  
Names are tied to Gender, Culture, and Background, and all data had been conveniently sourced from [this post by u/OrkishBlade on Reddit](https://www.reddit.com/r/DnDBehindTheScreen/comments/50pcg1/a_post_about_names_names_for_speakers_of_the/).  

**NOTES:** The probability of generating genders can also be manipulated using the ```pMale```, ```pFemale```, and ```pNonbinary``` arguments. These arguments have default values that roughly approximate their distributions in the real world, but can be manipulated to influence their probability of being generated. For example, we could conveniently note ```pMale = 0.70``` if we wanted to generate a distribution of simulated characters that skew male. In doing so, the probability of generating female and nonbinary characters would automatically shift to approximately 0.294 and 0.006 respectively. 

**FUTURE DIRECTIONS:** Future updates will aim to expand upon character simulation by adding more conventional (e.g., Race, Class, Height) and non-conventional characteristic options (e.g., Social connections, Occupational skills, Origins) to simulate. I also would like to continue to weigh the probability of generating these details based upon other concurrent details. For example, to be more realistic, the probability of generating characters of any given level should be exponentially inversely proportional to their level. Furthermore, it may be the case that characters of traditionally larger, bulky races may be more predisposed to classes and occupations where their size may come in handy (i.e., smithing, soldiers, manual labor). 

# **CombatSim**

**Summary:** A basic function which will simulate simple, melee combat between two or more opposing groups. The function only demands specification of the names of at least two groups (```Groups```), but allows grants the user the optional control to specify:  
1) Attack Bonuses,  
2) Damage Dice & Bonuses,  
3) Group Sizes,  
4) Armor Class,  
5) and the Hit Points of the parties.  
It will output a summary dataframe containing which combatants are still alive and what round the dead combatants fell. It will also output a summary message of the battle as well as, including who won, and how many are remaining among the fighting factions. It also has an option to output more granular, play-by-play results highlighting the actions each combatant took on their turn and what the results were.

**NOTES:** This function makes several assumptions for the sake of convenience. First, it assumes that all members of the same group share the same bonuses, damage dice, armor class, and hit points. It also assumes that all combatants begin at full health. It uses the [rollr package designed by Felixmil](https://github.com/Felixmil/rollR) to stimulate dice rolls and bonuses and may require downloading additional packages in its current state (notes included in the script).  

**FUTURE DIRECTIONS:** Ultimately, I would like to expand this to a point where it could function as a fully-fledged large-scale battle simulator. Future updates will aim to:  
1) provide an option to specify when the battle should end, whether that be after a certain number of rounds or once one group has been fully decimated. This would allow users to monitor a battle round by round or just jump straight to the end result.
2) expand the number of relevant variables to include weapons, shields, & ranges and correct initative rolls to better approximate that which would occur in a real tabletop RPG situation.
3) integrate the PersonSim and BattleSim packages so that users can conveniently and quickly simulate brand-new warriors with unique AC, HP, bonuses, and other relevant. variables. Variables could be constrained as much as desired.
4) make engagement more complex, taking into account positions within space and simple strategic considerations (e.g., low health, high AC).
5) build the option to simulate battles that include ranged physical combatants. 
6) provide conditions to use items (i.e., health potions) or take strategic actions (i.e., disengage, take cover).
7) integrate the ability to use unique class features or abilitites. 
8) integrate influence from support characters that may buff or heal combatants.
9) provide options to specify simple strategies that groups can use (e.g., flanking, ganging up, retreat at certain thresholds).
10) build the option to simulate battles that include magic-using combatants.
11) provide the option to bootstrap mutiple simulations and find the most likely outcome across several iterations.
