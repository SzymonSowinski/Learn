library(dplyr)
library(ggplot2)
library(gam)
library(plotly)

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
f = function (x)
  {(-20)*x+#gam_data$coefficients[2] +
                 x^2*(-70) +#gam_data$coefficients[3]+
                 x^3*6 #gam_data$coefficients[4]) 
  }

tmp = tibble(x = seq(0,15,0.1)) %>% 
  mutate(y = f(x))

c = c()

for (w in tmp$x){c = c(c,d(w,0.001))}

tmp =tmp %>% 
  mutate(yy = c)

ply = tmp%>% 
  ggplot()+
  geom_line(mapping = aes(x = x,
                          y = y))+
  geom_line(mapping = aes(x = x,
                          y = yy))

ggplotly(ply)

d =function(y, x, d = 0.001){
  sum(abs(f(seq(y+d,x+d, d)) - f(seq(y,x, d))))
}


d(5,10)

abs(f(5)-f(7.9))+abs(f(7.9) - f(10))

##########################

tmp = data[1:2,-4]


tmp = tibble(x_1 = c(1,1),
             x_2 = c(-5,5),
             x_3 = c(1,1),)

# 36.35852 - 36.31739  = 0.04113

predict(gam_data, tmp)

xx_1 = seq(tmp$x_1[1],tmp$x_1[2],0.01*sign(tmp$x_1[2]-tmp$x_1[1]))
xx_2 = seq(tmp$x_2[1],tmp$x_2[2],0.01*sign(tmp$x_2[2]-tmp$x_2[1]))
xx_3 = seq(tmp$x_3[1],tmp$x_3[2],0.01*sign(tmp$x_3[2]-tmp$x_3[1]))

max_len = max(length(xx_1),
              length(xx_2),
              length(xx_3))

xx_1_r = rep(xx_1, round(max_len/length(xx_1)-0.5 ))
xx_2_r = rep(xx_2, round(max_len/length(xx_2)-0.5 ))
xx_3_r = rep(xx_3, round(max_len/length(xx_3)-0.5 ))

colnames(data)


tmp = tibble(x_1 =sort(c(xx_1_r, sample(xx_1, size = max_len-length(xx_1_r))),
                       decreasing = (sign(tmp$x_1[2]-tmp$x_1[1])==1)),
             x_2 =sort(c(xx_2_r, sample(xx_2, size = max_len-length(xx_2_r))),
                       decreasing = (sign(tmp$x_2[2]-tmp$x_2[1])==1)),
             x_3 =sort(c(xx_3_r, sample(xx_3, size = max_len-length(xx_3_r))),
                       decreasing = (sign(tmp$x_3[2]-tmp$x_3[1])==1)))

p = predict(gam_data, tmp)

sum(abs(p-lag(p)), na.rm = T)

50.00868


plot(p)

