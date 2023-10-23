library(dplyr)
library(ggplot2)
library(gam)

data = tibble(x_1 = rnorm(1000),
              x_2 = rnorm(1000),
              x_3 = rnorm(1000),
              y = x_1 + x_2^2 + x_3^3/3 + 10 + rnorm(1000, sd = 0.01))


data %>% 
  ggplot()+
  geom_point(mapping = aes(x=x_1, y = y ))


gam_data = lm(y~poly(x_1, degree = 3, raw = TRUE)+
                 poly(x_2, degree = 3, raw = TRUE)+
                 poly(x_3, degree = 3, raw = TRUE),
               data = data)

summary(gam_data)

data %>% 
  mutate(pred_gam = predict(gam_data, data))



sum(c(1,
data$x_1[1],
data$x_1[1]^2,
data$x_1[1]^3,

data$x_2[1],
data$x_2[1]^2,
data$x_2[1]^3,

data$x_3[1],
data$x_3[1]^2,
data$x_3[1]^3)*gam_data$coefficients)


gam_data$coefficients[2]


# Expression or formula 
f = expression(x*0.9999551+#gam_data$coefficients[2] +
                 x^2*-2.042908e-05 +#gam_data$coefficients[3]+
                 x^3*0.0001288626 #gam_data$coefficients[4]) 
)

der = D(f, 'x')

der
exp(ax+b) - 1 = 0
