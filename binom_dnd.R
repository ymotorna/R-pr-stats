
adv_potion_15 <- dbinom(2, 2, 14/20)
adv_potion_15    # => 49% chance to roll < 15 twice (lose artifact)


buff_15 <- (14 - 4)/20
buff_15     # => 50% chance to lose artifact with charisma buffer (roll < 11)

# Summary
# after drinking advantage potion the probability to lose artifact is less than
# after using charisma buffer (49% < 50%), so I`ll choose the potion



# changes in test: need to roll 10+

adv_potion_10 <- dbinom(2, 2, 9/20)
adv_potion_10    # => 20% chance to lose


buff_10 <- (9 - 4)/20
buff_10       # => 25% chance to lose 



# changes in test: need to roll 18+

adv_potion_18 <- dbinom(2, 2, 17/20)
adv_potion_18    # => 72% chance to lose 


buff_18 <- (17 - 4)/20
buff_18        # => 65% chance to lose 

# Summary
# the greater number you need to roll, the less advantageous the potion is for you,
# so the boost should be chosen based om the difficulty of the test
























