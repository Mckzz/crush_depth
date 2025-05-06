###############     Sarah P's solution to making a curve     ###############



# Get the frequencies of collapse for each pressure and species
curve_species <- plyr::count(diameter.df, vars = c("species", "atm"))


# insert a row at the top of each species group that is the 
#### last pressure to have no collapse (100% survivourship): x atm for each species, 0freq
# Get the cumulative count of collapsed sacs per pressure measurement/ per species
curve_species <- curve_species %>%
  group_split(species) %>%
  map_dfr(~ .x %>%
            add_row(species = c("americanus", "trivittatus", "punctipennis"), 
                    atm = c(0.6, 1.0, 0.8), # pressures preceding first collapse for each species (for 100% remaining)
                    freq = 0,
                    .before = 1)) %>%
  dplyr::arrange(species, .by_group = T) %>%
  unique() %>%
  group_by(species) %>%
  mutate(cumulative_collapse = cumsum(freq)) %>%
  mutate(total_sacs = max(cumulative_collapse)) %>% # Calculate the total number of sacs for dividing for %
  mutate(pct.survivorship = (((total_sacs - cumulative_collapse) / total_sacs) * 100)) 

# Plot it
ggplot(curve_species, aes(x = atm, y = pct.survivorship, 
                          group = species, 
                          colour = species)) +
  geom_point() +
  #geom_smooth() +
  geom_line() +
  xlab("pressure (atm)") +
  ylab("% air-sacs surviving")


##############################################################################

### some other stuff

##########      survival curve
trivit <- diameter.df %>%
  filter(species == "trivittatus")

amer <- diameter.df %>%
  filter(species == "americanus")

punct <- diameter.df %>%
  filter(species == "punctipennis")

# a df starting just as the pressure sequence that goes to max pressure applied
press_seq <- data.frame(atm_axis = seq(0, 
                                       max(diameter.df$atm, na.rm = T), 
                                       by = 0.1))

# check an individual value
trivit$atm [trivit$atm > 2]
(sum((trivit$atm > 2) / length(trivit$atm)))


###############     Sarah P's solution to making a survivour curve     ###############

triv_collapse_pressure <- trivit$atm

# Get the frequencies of collapse for each pressure
curve <- plyr::count(triv_collapse_pressure)

# Rename the first column
names(curve)[1] <- "pressure_atm"

# insert a row at the top that is the last pressure to have no collapse (100% survivourship): 1atm, 0freq
curve <- add_row(curve, pressure_atm = 1, freq = 0, .before = 1)


# Get the count of collapsed sacs per pressure measurement
curve$cumulative_collapse <- cumsum(curve$freq)



# Calculate the total number of sacs
total_sacs <- length(triv_collapse_pressure)


# Calculate survivorship
curve$pct.survivorship <- ((total_sacs - curve$cumulative_collapse) / total_sacs) * 100

# Plot it
ggplot(curve, aes(x = pressure_atm, y = pct.survivorship)) +
  geom_point() +
  geom_smooth()


# number of trivittatus observations where collapse pressure is greater than the given pressure step, / total

totals <- count(diameter.df, species)

print(survivor)

nrow(diameter.df)


ggplot(diameter.df, aes(x = atm, 
                        colour = species,
                        linetype = instar)) +
  geom_density()

