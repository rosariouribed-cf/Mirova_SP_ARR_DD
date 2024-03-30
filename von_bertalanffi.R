# Fit a Von Bertalanffi model:

# Fit Model
fit_vonbert_model <- function(Age, AGB, ymax, b1_start = 0.05, b2_start = 1){
  vonlamb_model_out = nls(AGB ~ (ymax * 0.45) * (1-exp(-b1 * Age))^b2, start = list(b1 = b1_start, b2 = b2_start))
  return(vonlamb_model_out)
}

# Implement models as function of time
vonbert_model_f <- function(t, ymax, b1, b2){
  return((ymax * 0.45) * (1-exp(-b1 * t))^b2)
}

test_vonB <- fit_vonbert_model(Age, AGB = TAGB, ymax = 250, b1_start = 0.05, b2_start = 1)
coef(test_vonB)[2]
