library("plyr")
mlb <- read.csv("MLBRUNS2017.csv")
summary(mlb)

beta = lm(RUNS ~ WALKS + SINGLES + DOUBLES + TRIPLES + HOMERUNS + STOLEBASES + CAUGHTSTEAL + STRIKEOUTS, data = mlb)

summary(beta)

confint(beta,parm='STOLEBASES', level = 0.95)

coefficients <- coef(beta)
paste("RUNS = ", round(coefficients[1], 4), " + ", 
      round(coefficients['WALKS'], 4), "*WALKS + ", 
      round(coefficients['SINGLES'], 4), "*SINGLES + ", 
      round(coefficients['DOUBLES'], 4), "*DOUBLES + ", 
      round(coefficients['TRIPLES'], 4), "*TRIPLES + ", 
      round(coefficients['HOMERUNS'], 4), "*HOMERUNS + ", 
      round(coefficients['STOLENBASES'], 4), "*STOLENBASES + ", 
      round(coefficients['CAUGHTSTEAL'], 4), "*CAUGHTSTEAL + ", 
      round(coefficients['STRIKEOUTS'], 4), "*STRIKEOUTS", 
      sep = "")

output_text <- sapply(names(coefficients), function(var_name) {
  coef_value <- round(coefficients[var_name], 4)
  paste(coef_value, ": For every", var_name, "earned (", var_name, "increase by 1) the avg number of runs increases by", coef_value)
}, USE.NAMES = FALSE)

cat(output_text, sep = "\n")

triples_confint <- confint(beta, "HOMERUNS", level = 0.95)
print(triples_confint)

predict (beta,data.frame (mlb[30,]), interval="prediction", level =0.95)
