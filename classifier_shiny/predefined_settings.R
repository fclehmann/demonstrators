# predefined_settings.R

# reflecting real data of anthropometric data (estimated on approx. 6000 observations (2000 female, 4000 male)): 
# Gender mean_ft mean_kg sd_ft sd_kg  corr
# Female    24.6    67.8  1.24  11.0 0.536
# Male      27.1    85.5  1.31  14.2 0.484

# list of predefined settings
predefined_settings <- list(
  # e.g. children and adults
  setting_A = list(
    description = 'Einfachster Fall mit wenigen Beobachtungen, die sich perfekt klassifizieren lassen.',
    xlabel = 'Fußlänge in cm',
    ylabel = 'Gewicht in kg',
    group1_label = 'Frauen',
    group2_label = 'Männer',
    seed = 1,
    
    # group 1
    n_1 = 10,
    mean1_1 = 15, sd1_1 = 1.2,
    mean2_1 = 40, sd2_1 = 8,
    cor_1 = 0,
    
    # group 2
    n_2 = 10,
    mean1_2 = 25, sd1_2 = 1.6,
    mean2_2 = 80, sd2_2 = 8,
    cor_2 = 0
  ),
  # e.g. children and adults as A, but increasing n
  setting_B = list(
    description = 'Einfachster Fall mit vielen Beobachtungen, die sich perfekt klassifizieren lassen.',
    xlabel = 'Fußlänge in cm',
    ylabel = 'Gewicht in kg',
    seed = 1,
    
    # group 1
    n_1 = 100,
    mean1_1 = 15, sd1_1 = 1.2,
    mean2_1 = 40, sd2_1 = 8,
    cor_1 = 0,
    
    # group 2
    n_2 = 100,
    mean1_2 = 25, sd1_2 = 1.6,
    mean2_2 = 80, sd2_2 = 8,
    cor_2 = 0
  ),
  setting_C = list(
    description = 'Einfacher Fall mit wenigen Beobachtungen, die sich nicht perfekt klassifizieren lassen.',
    xlabel = 'Fußlänge in cm',
    ylabel = 'Gewicht in kg',
    seed = 1,
    
    # group 1
    n_1 = 10,
    mean1_1 = 24.6, sd1_1 = 0.84,
    mean2_1 = 67.8, sd2_1 = 11.0,
    cor_1 = 0.536,
    
    # group 2
    n_2 = 10,
    mean1_2 = 27.1, sd1_2 = 0.71,
    mean2_2 = 85.5, sd2_2 = 14.2,
    cor_2 = 0.484
  ),
  setting_D = list(
    description = 'Einfacher Fall mit vielen Beobachtungen, die sich nicht perfekt klassifizieren lassen.',
    xlabel = 'Fußlänge in cm',
    ylabel = 'Gewicht in kg',
    seed = 1,
    
    # group 1
    n_1 = 50,
    mean1_1 = 24.6, sd1_1 = 0.74,
    mean2_1 = 67.8, sd2_1 = 11.0,
    cor_1 = 0.536,
    
    # group 2
    n_2 = 50,
    mean1_2 = 27.1, sd1_2 = 1.01,
    mean2_2 = 85.5, sd2_2 = 14.2,
    cor_2 = 0.484
  )
  # Add more settings as needed
)
