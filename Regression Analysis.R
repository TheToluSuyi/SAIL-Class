galton <- Galton
Galton
par(mfrow=c(1,2))
hist(galton$child,breaks=100, col = "lightblue", border = "pink")
hist(galton$parent,breaks=100, col = "lightblue", border = "pink")
ggplot(galton, aes(x = child)) + 
  geom_histogram(binwidth = 1, colour = "lightblue", fill = "lightblue") +
  geom_vline(xintercept = mean(galton$child), colour = "blue", lwd = 5)
ggplot(galton, aes(parent, child) ) +
  geom_point()
ggplot(galton, aes(parent, child))+
  geom_point()+ 
  geom_smooth(method = "lm", se =FALSE)
lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)
freq_galton <- as.data.frame(table(galton$child, galton$parent))

names(freq_galton) <- c("child", "parent", "freq")

plot(as.numeric(as.vector(freq_galton$parent)), 
     as.numeric(as.vector(freq_galton$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .05 * freq_galton$freq, 
     xlab = "Parents", ylab = "Children")

lm1 <- lm(galton$child ~ galton$parent)
lines(galton$parent,lm1$fitted,col="red",lwd=3)
diamond <- diamonds
ggplot(diamond, aes(x = carat, y = price)) +
  geom_point(color = "black", size = 1.1, shape = 21) +
  geom_smooth(method = lm, se = FALSE, col = "darkblue", linewidth = 1.1) + 
  theme_classic() + 
  theme(panel.background = element_rect(fill = "lightblue")) +
  xlab("Mass (Carats)") + 
  ylab("Price (SIN $)")
fit <- lm(price ~ carat, data = diamond)
coef(fit)
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)
view(diamond)
round(coef(fit)[1], 2)
round(coef(fit)[2], 2)
data(diamond)

y <- diamond$price; x <- diamond$carat; n <- length(y)

fit <- lm(y ~ x)

e <- resid(fit)

yhat <- predict(fit)

ggplot(data = diamond, aes(x = carat, y = price)) +
  geom_point(shape = 21, color = "blue", size = 1.4) +
  geom_abline(lwd = 2) +
  geom_line(aes(x = carat, y = yhat, color = "red", lwd = 2)
  
abi <- runif(100, -3, 3); sola <- abi + sin(abi) + rnorm(100, sd = .2)
library(ggplot2)
ggplot(data = data.frame(abi, sola), aes(x = abi, y = sola)) + 
    geom_point() + 
    geom_smooth(method = "lm") 
x <- runif(100, 0, 6); y <- x + rnorm(100,  mean = 0, sd = .001 * x); 
library(ggplot2)
ggplot(data = data.frame(x, y), aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm")
x <- runif(100, 0, 6); y <- x + rnorm(100,  mean = 0, sd = .001 * x); 

ggplot(data = lm(y ~ x)) + 
  geom_point(aes(x = x, y = resid(lm(y ~ x)))) + 
  geom_hline(aes(yintercept = 0))
fit <- lm(price ~ carat, data = diamond)
autoplot(fit,
         which = 1:4,
         nrow = 4,
         ncol = 1
)
fit %>%
  augment() %>%
  select(price, carat, leverage = .hat) %>%
  arrange(desc(leverage)) %>%
  head()
cooks.distance(fit)
library(dplyr)
library(broom)  # Assuming you want to use broom's augment() function

fit %>%
  augment() %>%
  select(price, carat, leverage = .hat) %>%
  arrange(desc(leverage)) %>%
  head()
mtcars
model <- lm(mpg ~ wt + cyl, data = mtcars)
model$coefficients
summary(model)
head(churning)
#############################
galton
ggplot(galton, aes(x = child)) + 
  geom_histogram(binwidth = 1, colour = "lightblue", fill = "lightblue") +
  geom_vline(xintercept = mean(galton$child), colour = "blue"
lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)
ggplot(galton, aes(parent, child) ) +
  geom_point()            
ggplot(galton, aes(parent, child))+
  geom_point()+ 
  geom_smooth(method = "lm", se =FALSE)
freq_galton <- as.data.frame(table(galton$child, galton$parent))
names(freq_galton) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freq_galton$parent)), 
     as.numeric(as.vector(freq_galton$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .05 * freq_galton$freq, 
     xlab = "Parents", ylab = "Children")
lm1 <- lm(galton$child ~ galton$parent)
lines(galton$parent,lm1$fitted,col="red",lwd=3)
##########################################
diamond
ggplot(diamond, aes(x = carat, y = price)) +
  geom_point(color = "black", size = 1.1, shape = 21) +
  geom_smooth(method = lm, se = FALSE, col = "darkblue", linewidth = 1.1) + 
  theme_classic() + 
  theme(panel.background = element_rect(fill = "lightblue")) +
  xlab("Mass (Carats)") + 
  ylab("Price (SIN $)")
fit <- lm(price ~ carat, data = diamond)
coef(fit)
round(coef(fit)[1], 2)
round(coef(fit)[2], 2)
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)
library(dplyr)
fit <- lm(price ~ carat, data = diamond)
set.seed(234)
newX <- tibble(runif(48, 0.2, 0.5))
newX <- round(newX, 2)
coef(fit)[1] + coef(fit)[2] * newX
predict(fit, carat = newX)
price_predicted = round(predict(fit, carat = newX), 2)
#price_predicted
price_predicted2 <- diamond %>% 
  mutate(prediction = round(predict(fit, carat = newX), 2))
price_predicted2
fit %>% 
  glance()
diamond %>%
  summarise(
    coeff_determination = cor(price, carat) ^ 2
  )
fit <- lm(price ~ carat, data = diamond)
fit
#################
library(dplyr)
fit <- lm(price ~ carat, data = diamond)

set.seed(234)
newX <- tibble(runif(48, 0.2, 0.5))
newX <- round(newX, 2)
coef(fit)[1] + coef(fit)[2] * newX
# 
predict(fit, carat = newX)


price_predicted = round(predict(fit, carat = newX), 2)

#price_predicted

price_predicted2 <- diamond %>% 
  mutate(prediction = round(predict(fit, carat = newX), 2))

price_predicted2
